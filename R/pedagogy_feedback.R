#' Pedagogy Feedback
#'
#' The pedagogy_feedback() function is used to generate feedback from the pedagogy website.
#'
#' @param connection Pedagogy database connection
#' @param slack_channel Slack channel name
#' @param slack_username Slack username
#' @param slack_token Slack token
#' @param workshop_id Id of Algoritma workshop
#'
#' @return send the summary of workshop to selected slack channel
#' @export
#'
#' @examples
#' pedagogy_feedback(connection = cn,
#'           slack_channel = "datascience",
#'           slack_username = "David",
#'           slack_token = "x123yz",
#'           workshop_id = 890)
#'
pedagogy_feedback <- function(connection, slack_channel, slack_username, slack_token, workshop_id){
  ## Slack Setup
  slackr::slackrSetup(channel = slack_channel,
                      username = slack_username,
                      api_token = slack_token)

  ## Query

  workshop <- DBI::dbGetQuery(conn = connection,
                         statement = paste0("SELECT workshop.id as workshop_id, workshop_name, workshop_category, workshop_start AS date, workshop_venue, class_size, name as instructor,
                                                  count(workshop.id) as number_responden,
                                                  avg(difficulty) as difficulty,
                                                  avg(assistants_score) as assistants_score,
                                                  avg(knowledge) as knowledge,
                                                  avg(objectives) as objectives,
                                                  avg(timeliness) as timeliness,
                                                  avg(venue_score) as venue_score,
                                                  avg(satisfaction_score) as satisfaction_score,
                                                  sum(case when satisfaction_score=5 then 1 else 0 end) as overall
                                          FROM workshop
                                          LEFT JOIN employee on employee.id = workshop.workshop_instructor
                                          LEFT JOIN response on response.workshop_id = workshop.id
                                          WHERE workshop.id = ", workshop_id)) %>%
    dplyr::mutate(date = lubridate::ymd_hms(date))

  assistant <- DBI::dbGetQuery(conn = connection,
                          statement = paste0("SELECT workshop_id, employee.name as assistants
                                            FROM assistants
                                            LEFT JOIN employee on assistants.employee_id = employee.id
                                            WHERE workshop_id = ", workshop_id))



  ftable_id <- workshop %>%
    dplyr::select(workshop_id, workshop_name,
           workshop_category, date,
           workshop_venue, class_size,
           instructor, number_responden)

  TA <- paste0(assistant$assistants, collapse = ", ")


  ftable <- workshop %>%
    dplyr::select(-c(1:8)) %>%
    tidyr::pivot_longer(cols = everything(.),
                        names_to = "Statement",
                        values_to = "Rating") %>%
    dplyr::mutate(Rating = round(Rating,1),
           Statement = dplyr::recode(Statement,
                              difficulty = "The lecture presented wasn't too difficult",
                              assistants_score = "The teaching assistants were helpful",
                              knowledge = "The trainer was knowledgeable about the training topics",
                              objectives = "My training objectives were met",
                              timeliness = "The time allocated for the training was sufficient",
                              venue_score = "Training venue and facilities were adequate and comfortable",
                              satisfaction_score = "Overall",
                              overall = "5+ Satisfaction")) %>%
    `row.names<-`(., NULL) %>%
    tibble::column_to_rownames(var = "Statement")


## blm beres
  # add aesthetic theme --------------------------------------------------

  if(Sys.info()[[1]]=="Windows"){
    windowsFonts("Fjalla One" = windowsFont("Fjalla One"))
    windowsFonts("Roboto Condensed" = windowsFont("Roboto Condensed"))
    windowsFonts("Roboto Medium" = windowsFont("Roboto Medium"))
  }

  # ???
  g_theme <- gridExtra::ttheme_minimal(core =list(fg_params = list(fontface=c(rep("plain")))),
                            rowhead = list(fg_params=list(x=0, hjust=0, fontface="plain")),
                            base_colour = "white",
                            base_size = 8.6,
                            base_family = "Roboto Medium")

  #custome theme
  gp_theme <- ggplot2::theme(plot.title = ggplot2::element_text(face="plain",
                                              size=14,
                                              hjust=0,
                                              colour = "#d63e2d",
                                              family = "Fjalla One"),
                    plot.subtitle = ggplot2::element_text(size=10,
                                                 hjust=0),
                    plot.caption = ggplot2::element_text(size=9,
                                                hjust=1),
                    plot.margin = ggplot2::margin(0.15, 0.15, 0.15, 0.15, "cm"),
                    panel.background = ggplot2::element_rect(fill = "black",
                                                    colour="white"),
                    text = ggplot2::element_text(family="Roboto Condensed"))

  # create labels for plot
  gp_title <- ggplot2::labs(title = stringr::str_to_upper(paste0(ftable_id$workshop_category,": ",
                                               ftable_id$workshop_name)),
                   subtitle = paste("at",ftable_id$workshop_venue,"\n",
                                    ftable_id$date),
                   caption = paste("Number of Respondents:", ftable_id$number_responden,
                                   "/",ftable_id$class_size,"\n",
                                   "Instructor:",ftable_id$instructor,"\n",
                                   "Assistants:",TA))

  # finalizing --------------------------------------------------------------

  fgg <- gridExtra::tableGrob(ftable,
                              theme = g_theme)

  # buat garis

  separators <- replicate(ncol(fgg),
                          grid::segmentsGrob(x1 = grid::unit(0, "npc"),
                                             gp= grid::gpar(lty = 1,
                                               col = "white")),
                          simplify=FALSE)


  fgg_pub <- fgg %>%
    gtable::gtable_add_grob(grobs = separators,
                    t = 1,
                    b = nrow(fgg),
                    l = 2,
                    r = 2) %>%
   ggplotify::as.ggplot()+
    gp_title +
    gp_theme


  # save plot to directory
  ggplot2::ggsave(stringr::str_replace_all(paste0(workshop$workshop_name,".png")," ",""), height = 10, width = 12, units = "cm")
  # send to slack -----------------------------------------------------------

  #send file to slack
  slackr::slackr_upload(filename = stringr::str_replace_all(paste0(workshop$workshop_name,".png")," ",""),
                        channels = slack_channel,
                        api_token = slack_token)

  #delete plot from directory
  unlink(stringr::str_replace_all(paste0(ftable_id$workshop_name,".png")," ",""))

}
