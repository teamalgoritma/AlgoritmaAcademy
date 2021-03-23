library(dplyr)
#' Zoom Attendance List
#'
#'The zoom_attendance() function is used to summarizes the attendance of students in classes conducted via zoom
#'
#' @param folder_name The name of folder where attendance lists are stored
#' @param sheet_url Google spreadsheet link (string).
#' @param sheet_name Sheet name on the spreadsheet (string).
#' @param email your email (string). for authentication purpose.
#'
#' @return  "Attendance Updated Successfully" which mean the score on the spreadsheet is updated.
#' @export
#'
#' @examples
#' zoom_attendance(folder_name = "midas RM Day 3",
#' email = "david_at_algorit.ma",
#' sheet_url = "https://docs.google.com/spreadsheets/d/xx/yy",
#' sheet_name = "RM Day 3")
#'
zoom_attendance <- function(folder_name, sheet_url, sheet_name, email) {
  # Detect the files in folder
  files_name <- list.files(folder_name)

  # Read and join the files
  df_absen <- NULL
  for (i in 1:length(files_name)) {
    temp <- readr::read_csv(paste0(folder_name,"/",files_name[i]))
    df_absen <- dplyr::bind_rows(df_absen,temp)
  }

  # Clean the data
  df_absen <- df_absen %>%
    janitor::clean_names() %>%
    dplyr::rename(name = 1) %>%
    dplyr::filter(name %>% stringr::str_detect("]") == F) %>%
    dplyr::mutate_at(dplyr::vars(join_time, leave_time),lubridate::mdy_hms) %>%
    dplyr::mutate(class = ifelse(lubridate::hour(join_time) < 17, "Day", "Night")) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(join_time = min(join_time),
           leave_time = max(leave_time)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(duration = difftime(leave_time, join_time, units = "mins") %>%
             as.numeric() %>%
             round(3)
    ) %>%
    dplyr::select(class,
           name,
           Join.Time = join_time,
           Leave.Time = leave_time,
           duration) %>%
    dplyr::arrange(class, name)

  # Write to sheet
  googlesheets4::gs4_auth(email = email)
  googlesheets4::write_sheet(data = df_absen,
              ss = sheet_url,
              sheet = sheet_name)
  googlesheets4::gs4_deauth()
  print("Attendance Updated Successfully")
}
