#' Update Timeline
#'
#' The update_timeline() function is used to update timeline_dev sheet on piket report spreadsheet.
#' The source of data is from schedule workshop sheet and piket report sheet.
#'
#' @param schedule_row row number of "schedule workshop" sheet (integer). The default is 0 which means the timeline sheet will not be updated from this sheet
#' @param mentoring_row row number of "piket report" sheet (integer). The default is 0 which means the timeline sheet will not be updated from this sheet
#' @param email your algoritma email (string)
#'
#' @return "Timeline_dev" sheet on "piket report" spreadsheet updated
#' @export
#'
#' @examples
#' update_timeline(schedule_row = 944,
#' mentoring_row = 440,
#' email = "David_at_algorit.ma")
update_timeline <- function(schedule_row = 0, mentoring_row = 0, email) {
  # load libs ---------------------------------------------------------------
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(googlesheets4)
  library(lubridate)
  library(zoo)
  gs4_auth(email = email)

  # load data ---------------------------------------------------------------
  schedule_url <- "1APwoLJ4lGGNnYhOfQ9AVF14f-aSmNDmAeA0PtMYwMIc"
  piket_url <-  "1OMT9kAU_ceoku4M44VZWqiA1al9tKpJzpN6rfY5hLzY"

  ## start and end workshop
  workshop_time <- read_sheet(ss = schedule_url,
                              sheet = "Reference",
                              range = "Reference!A1:C") %>%
    mutate(Name = tolower(Name))

  ## product team names and piket team
  product_team <- read_sheet(ss = schedule_url,
                             sheet = "Reference",
                             range = "Reference!E1:F") %>%
    mutate_all(tolower)

  ## Get name only
  team <- product_team$Nama %>%
    paste(collapse = "\\b|\\b")

  # nama hari
  day_name <- c("senin", "selasa", "rabu",
                "kamis", "jumat", "sabtu", "minggu")


  # Read data ---------------------------------------------------------------

  if (schedule_row == 0) {

  }
  else{
    schedule2 <- read_sheet(ss = schedule_url,
                            sheet = "Jadwal",
                            skip = schedule_row-1,
                            col_names = F) %>%
      mutate_all(
        function(x) map_chr(x, function(x) ifelse(is.null(x), NA, x) %>% as.character() %>% tolower())
      )

    ## Cek jumlah kolom
    if (ncol(schedule2)>7) {
      schedule2 <- schedule2 %>%
        select(1:7)
      names(schedule2) <- day_name[1:ncol(schedule2)]
    }else(
      names(schedule2) <- day_name[1:ncol(schedule2)]
    )

    ## Convert into tidy format
    date_base <- schedule2[1,1]
    timeline2 <- schedule2 %>%
      pivot_longer(cols = names(.)) %>%
      mutate_if(is.character, tolower) %>%
      mutate(value = case_when(value %in% c("bibil", "nabil","nabiilah") ~ "nabiilah",
                               value %in% c("doyo","hadoyo") ~ "handoyo",
                               value == "echa"~ "eca",
                               value == "allisa"~ "alisa",
                               TRUE ~ value),
             person = ifelse(str_detect(value, team), value,NA),
             Date = ifelse(value %in% 1:31, value,NA),
             workshop = ifelse(is.na(person) & is.na(Date), value, NA)
      ) %>%
      filter(!is.na(value)) %>%
      group_by(name) %>%
      arrange(desc(name)) %>%
      mutate(Date = na.locf(Date) %>%
               as.numeric()) %>%
      filter(nchar(x = value)>2) %>%
      mutate(
        month = ifelse(Date < date_base, month(Sys.Date())+1 ,month(Sys.Date())),
        Date = paste(Date,month,year(Sys.Date())) %>% dmy()
      ) %>%
      select(-month) %>%
      mutate(workshop = na.locf(workshop)) %>%
      filter(value != workshop) %>% # remove workshop name from value
      ungroup() %>%
      select(-value)


    ##Joining data
    timeline_final2 <- timeline2 %>%
      left_join(product_team, by = c("person" = "Nama")) %>%
      mutate(workshop_name = str_extract(string = workshop, pattern = workshop_time$Name %>%
                                           paste(collapse = "\\b|\\b"))) %>%
      left_join(workshop_time, by = c("workshop_name" = "Name")) %>%
      transmute(Nama = str_to_title(person),
                Team = toupper(Team),
                Date,
                Start, End,
                Notes = toupper(workshop)) %>%
      filter(!is.na(Team)) %>%
      arrange(Date, Nama)

    ## Insert data into sheet

    timeline_final2 %>%
      sheet_append(ss = piket_url,
                   sheet = "Timeline_Dev")


  }

  # Mentoring ---------------------------------------------------------------

  if (mentoring_row ==0) {

  }
  else{
    mentoring_names <- c('ID','Ticket','Date Created',
                         'Name','Email','Class',
                         'Material','Notes','Status',
                         'Mentor','Assigned Date',
                         'Start','End','Team','Period')

    mentoring <- read_sheet(ss = piket_url,
                            sheet = "Mentoring",
                            skip = mentoring_row-1,
                            col_names = mentoring_names,
                            col_types = rep("c",15) %>%
                              paste(collapse = "")
    )

    mentoring_final <- mentoring %>%
      transmute(Nama = Mentor,
                Team,
                Date = ymd_hms(`Assigned Date`),
                Start = hour(Date),
                End = Start + 1,
                Notes = paste('MENTORING'),
                Date = as.Date(Date)) %>%
      filter(year(Date) > 2020) %>%
      drop_na() %>%
      arrange(Date,Nama)

    # Write to sheet

    mentoring_final %>%
      sheet_append(ss = piket_url,
                   sheet = "Timeline_Dev")


  }
}
