#' Recapitulation of Student Attendance
#'
#'The recap_attendence() function is used to summarizes the attendance list from zoom and gform sheet
#'
#' @param gform_sheet Google Form spreadsheet link (string)
#' @param absen_sheet "Rekap Absensi Academy" spreadsheet link (string)
#' @param absen_name Sheet name on "Rekap Absensi Academy" (string)
#' @param zoom_sheet "Rekap Absensi Zoom" spreadsheet link (string)
#' @param zoom_name Sheet name on "Rekap Absensi Zoom" (string)
#' @param email Your algoritma email
#'
#' @return Student whose username was not found in the class
#' @export
#'
#' @examples
#'
#' recap_attendence(gform_sheet =  "https://docs.google.com/spreadsheets/d/xxx/yyy",
#' absen_sheet = "https://docs.google.com/spreadsheets/d/aaa/bbb",
#' absen_name =  "Newton DA Night",
#' zoom_sheet = "https://docs.google.com/spreadsheets/d/zzz/www",
#' zoom_name = "P4DS Day 2", email = "david_at_algorit.ma")
#'
recap_attendence <- function(gform_sheet, absen_sheet,
                             absen_name, zoom_sheet,
                             zoom_name, email){

  # load libs ---------------------------------------------------------------
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(padr)
  library(zoo)
  library(googlesheets4)
  googlesheets4::gs4_auth(email = email)

  # Create academy calender data frame --------------------------------------
  df_kalender <- read_sheet(ss = gform_sheet, sheet = "kalender")
  names(df_kalender) <- tolower(names(df_kalender))

  df_kalender <- df_kalender %>%
    mutate_at(vars(c("mulai", "selesai")), dmy)

  df_date <- df_kalender %>%
    pivot_longer(cols = c(mulai, selesai),
                 names_to = "type",
                 values_to = "date") %>%
    nest(-c(cohort, spesialisasi, kelas)) %>%
    mutate(data =
             map(.$data, function(x) x %>%
                   pad(interval = "day",
                       start_val = min(x$date),
                       end_val = max(x$date))
             )
    ) %>%
    unnest() %>%
    select(-type) %>%
    left_join(df_kalender)


  # Output 1: Rekap absensi dari google form --------------------------------
  ## Read rekap absen
  df_rekap <- read_sheet(ss = absen_sheet, sheet = absen_name) %>%
    drop_na(No)

  ## Get student names only
  student_name <- df_rekap %>%
    pull("Student Full Name")

  ## Ambil tanggal-tanggal kalender akademik sebelum hari ini
  date_col <- df_date %>%
    filter(date < Sys.Date()) %>%
    pull(date) %>%
    strftime(format = "%d/%m/%y")

  ## ambil tanggal yang belum ada di sheet rekap absen
  rm_col <- date_col[!date_col %in% names(df_rekap)]

  ## Read Gform
  df_form <- read_sheet(ss = gform_sheet)

  ## Convert Data from Gform into Rekap Absen sheet format
  df_absen <- df_form %>% # dari google form
    filter(Name %in% student_name) %>% # dari spreadsheet rekap absensi
    mutate(date = as.Date(Timestamp)) %>% # diasumsikan tanggal isi form = tanggal kelas
    select(Name, date, Day) %>%
    left_join(df_date ,
              by = "date") %>% # dari kalender akademik
    na.locf() %>% # jika mengisi di luar tanggal workshop diasumsikan sama dengan tanggal sebelumnya
    distinct() %>% # Menghapus absensi 2 kali untuk tanggal workshop yang sama
    mutate(tanggal = mulai + days(Day) - days(1)) %>%
    # filter(tanggal >= (Sys.Date() - days(7))) %>% # sesuai input
    select(-c(cohort, spesialisasi, kelas, date, mulai, selesai)) %>%
    distinct() %>%
    arrange(tanggal) %>%
    mutate(tanggal = strftime(tanggal, format = "%d/%m/%y")) %>%
    pivot_wider(id_cols = Name, names_from = tanggal, values_from = Day) %>%
    mutate_at(vars(2:ncol(.)), function(x) ifelse(is.na(x), "-", "y")) %>%
    arrange(Name) %>%
    rename("Student Full Name" = Name) %>%
    select("Student Full Name", rm_col) %>%
    right_join(df_rekap, by = c("Student Full Name")) %>%
    select(No, "Student Full Name", date_col) %>%
    mutate_at(vars(date_col), na.fill, fill = "-") %>%
    arrange(No)

  # Output 1 ---------------------------------------------------------------
  df_absen %>%
    write_sheet(ss = absen_sheet,
                sheet = absen_name)


  # Output 2: Dissimilarity -------------------------------------------------

  ## Ambil kalender untuk timeline
  df_date <- df_date %>%
    mutate(day = as.numeric(date - mulai) + 1,
           agenda = paste(kelas, "Day", day),
           tanggal = strftime(date, format = "%d/%m/%y")
    )

  ## Read rekap absensi zoom
  df_zoom <- read_sheet(zoom_sheet, sheet = zoom_name)

  ## Normalize the student name and joining with df_date
  df_zoom2 <- df_zoom %>%
    mutate(name = tolower(name) %>%
             str_replace_all("[:punct:]", " ") %>%
             str_replace_all("[0-9]", " ") %>%
             str_replace_all("[|]", " ") %>%
             str_squish() %>%
             str_trim(),
           date = as.Date(Join.Time)) %>%
    left_join(df_date) %>%
    select(name, date, cohort, spesialisasi, kelas) %>%
    arrange(name) %>%
    distinct()

  ## Get the student patterns name
  df_zoom_name <- df_zoom2 %>%
    pull(name) %>%
    paste("\\b", ., "\\b", sep = "", collapse = "|")

  regex_name <- df_zoom2$name %>%
    str_split(" ") %>%
    map(function(x) {

      name_stud <- x[ nchar(x) > 2] %>%
        head(3) %>%
        paste0("(?=.*", .,")")

      if (length(name_stud) > 1) {
        name_stud <- name_stud %>%
          combn(m = 2) %>%
          apply(MARGIN = 2, paste, collapse = "", sep="") %>%
          paste( "(",., ")" , sep = "", collapse = "|")
      }
      return(name_stud)
    }) %>%
    paste(sep = "", collapse = "|")

  selected_agenda <- df_date %>%
    filter(agenda == zoom_name) %>%
    pull(tanggal)

  df_for_zoom <- df_absen %>%
    rename(gform_sheet =  selected_agenda , # Tanggal workshop
           Name = "Student Full Name") %>%
    select(Name, gform_sheet) %>%
    mutate(Name = tolower(Name) %>%
             str_replace_all("[:punct:]", " ") %>%
             str_replace_all("[0-9]", " ") %>%
             str_replace_all("[|]", " ") %>%
             str_squish() %>%
             str_trim()
    )

  googlesheets4::gs4_deauth()

    final <- df_for_zoom %>%
    filter(str_detect(Name, regex_name, negate = T))
  return(final)
}
