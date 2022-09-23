#' Formats the staff spreadsheet to be merged with the e-learning
#'
#' @param staff the excel spreasheet downloaded with the staff counts
#'
#' @return dataframe with relevant variables
#' @export
#'
#' @examples
staff_df <- function(staff) {
  read_excel(staff, sheet = 2) %>% clean_names() %>%  select(person_number, name, group, directorate, division) %>%
    dplyr::rename(ID = person_number, area = group) %>%
    transform(ID = as.numeric(ID), area = as.factor(area), directorate = as.factor(directorate), division = as.factor(division))
}




#' Title
#'
#' @param x course dataset
#' @param t cut off date in the form of YYYY-MM-DD, e.g.,2022-07-31
#'
#' @return dataframe with relevant variables
#' @export
#'
#' @examples
course_foo <- function(x, t) {
  read_excel(x) %>%clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date("t")) %>% select(-Time, -Date)
}



#' Merged datasets
#'
#' @param x course formatted dataset
#' @param y staff formatted dataset
#'
#' @return combined dataset
#' @export
#'
#' @examples
merge_foo <- function(x,y) {
  left_join(x,y) %>% replace_na(list(attempt = 0))
}




#' Completion rate for ONS
#'
#' @param x merged dataset returned from function merged
#'
#' @return
#' @export
#'
#' @examples
prc_foo <- function (x) {
  prc <-sum(x$attempt)/nrow(x)*100
  round(prc,1)
}
