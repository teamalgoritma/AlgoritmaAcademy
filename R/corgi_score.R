library(dplyr)

#' Corgi Quiz Assessment
#'
#'
#' The corgi_score() function is used to assess the Algoritma students based on the quiz on corgi.re.
#'
#' @param corgi_url  Link to course on corgi.re (string).
#' @param sheet_url Google spreadsheet link (string).
#' @param sheet_name Sheet name on the spreadsheet (string).
#' @param column_name The column that you want to update (string).
#' @param max_score Maximum score on the quiz (numeric).
#' @param email your email (string). for authentication purpose.
#'
#' @return "Quiz Updated Successfully" which mean the score on the spreadsheet is updated.
#' @export
#'
#' @examples
#' corgi_score(corgi_url = "https://corgi.re/courses/Davidlimbong/P4DS-PS",
#'       sheet_url = "https://docs.google.com/spreadsheets/d/xxxxx/yyyyy",
#'       sheet_name = "Academy: Batch 9",
#'       column_name = "NN Quiz"
#'       max_score = 4,
#'       email = "david_at_algorit.ma")
#'
corgi_score <- function(corgi_url, sheet_url, sheet_name, column_name, max_score, email) {

  getColumnName <- function(word_length) {

    if(word_length <= 26){
      col_id <- paste0(LETTERS[word_length], 1)
    }else{

      if(floor(word_length/26)<=26){
        start <- LETTERS[floor(word_length/26)]
        col_id <- paste0(start, end,1)
      }

    }
    return(col_id)
  }

# Get recipients from corgi

  ## Get participant name
  corgi <- xml2::read_html(corgi_url) %>%
    rvest::html_nodes(xpath = "//a") %>%
    rvest::html_text()
  corgi <- tolower(corgi[12:(length(corgi)-10)])

  # Read and write to sheet
  ## Read sheet
  googlesheets4::gs4_auth(email = email)
  academy <- googlesheets4::read_sheet(ss = sheet_url, sheet = sheet_name)
  id <- match(column_name, names(academy))

  academy %>%
    dplyr::filter(!is.na(Class)) %>%
    dplyr::mutate(`Github ID` = tolower(`Github ID`)) %>%
    transmute_at(.vars = dplyr::vars(dplyr::contains(column_name)),
                 .funs = function(x) ifelse(.$`Github ID` %in% corgi, max_score,0)) %>% ## Scoring
    googlesheets4::range_write(ss = sheet_url,
                sheet = sheet_name,
                range = getColumnName(id)) # write the result
  googlesheets4::gs4_deauth()
  print("Quiz Updated Successfully")

}
