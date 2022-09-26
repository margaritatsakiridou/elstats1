#' Completion rates for ONS
#'
#' @param staff path to staff excel spreadsheet
#' @param course path to course logs downloaded
#' @param date cut off date as string in the format "YYYY-MM-DD"
#'
#' @return percentage completion rate
#' @export
#'
#' @examples ons_completion_rate(staff = staff, course = course, date = date)

ons_completion_rate <- function (staff, course, date) {

  staff_df <- read_excel(staff, sheet = 2) %>% clean_names() %>%  select(person_number, name, group, directorate, division) %>%
    dplyr::rename(ID = person_number, area = group) %>%
    transform(ID = as.numeric(ID), area = as.factor(area), directorate = as.factor(directorate), division = as.factor(division))

  course_df <- read_excel(course) %>% clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date(date)) %>% select(-Time, -Date)

  combined <- left_join (staff_df, course_df) %>% replace_na(list(attempt = 0))

  prc <-sum(combined$attempt)/nrow(combined)*100
  round(prc,1)
}


#'Total that have completed the course and total employees for reporting purposes
#'
#' @param staff path to staff excel spreadsheet
#' @param course path to course logs downloaded
#' @param date cut off date as string in the format "YYYY-MM-DD"
#'
#' @return two values; the first is the number of employees that have completed the course and the second is the total employees
#' @export
#'
#' @examples checks total (staff = staff, course = course, date=date)
#'
checks_total <- function (staff, course, date) {
  staff_df <- read_excel(staff, sheet = 2) %>% clean_names() %>%  select(person_number, name, group, directorate, division) %>%
    dplyr::rename(ID = person_number, area = group) %>%
    transform(ID = as.numeric(ID), area = as.factor(area), directorate = as.factor(directorate), division = as.factor(division))

  course_df <- read_excel(course) %>% clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date(date)) %>% select(-Time, -Date)

  combined <- left_join (staff_df, course_df) %>% replace_na(list(attempt = 0))

  total_course <- print(sum(combined$attempt))
  total_ons <- print(nrow(combined))
}


#Function 3


#' Completion rate for each employee
#'
#' @param staff path to staff excel spreadsheet
#' @param course1 path to course logs downloaded for course1
#' @param course2 path to course logs downloaded for course2
#' @param date cut off date as string in the format "YYYY-MM-DD"
#'
#' @return a dataframe with information on completion per employee
#' @export
#'
#' @examples individual_completions(staff = staff, course1 = course1, course2 = course2, date = date)
individual_completions <- function(staff, course1, course2, date) {

  staff_df <- read_excel(staff, sheet = 2) %>% clean_names() %>%  select(person_number, name, group, directorate, division) %>%
    dplyr::rename(ID = person_number, area = group) %>%
    transform(ID = as.numeric(ID), area = as.factor(area), directorate = as.factor(directorate), division = as.factor(division))

  course1_df <- read_excel(course1) %>% clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date(date)) %>% select(-Time, -Date)

  course2_df <- read_excel(course2) %>% clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date(date)) %>% select(-Time, -Date)

  combined1 <- left_join (staff_df, course1_df) %>% replace_na(list(attempt = 0))
  combined2 <- left_join (staff_df, course2_df) %>% replace_na(list(attempt = 0))

  both <- left_join(combined1, combined2, by= "ID") %>% select(1:6,11)
  colnames(both) <- c("ID", "name", "area", "directorate", "division", "course1", "course2")
  both$both <- ifelse((both$course1 == 1 & both$course2 == 1), 1,0)
  both
}



#' Completion rate for each area (of interest)
#'
#' @param staff path to staff excel spreadsheet
#' @param course1 path to course logs downloaded for course1
#' @param course2 path to course logs downloaded for course2
#' @param date cut off date as string in the format "YYYY-MM-DD"
#'
#' @return a dataframe with information on completion per division
#' @export
#'
#' @examples
divisions_completions <- function (staff, course1, course2, date) {
  staff_df <- read_excel(staff, sheet = 2) %>% clean_names() %>%  select(person_number, name, group, directorate, division) %>%
    dplyr::rename(ID = person_number, area = group) %>%
    transform(ID = as.numeric(ID), area = as.factor(area), directorate = as.factor(directorate), division = as.factor(division))

  course1_df <- read_excel(course1) %>% clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date(date)) %>% select(-Time, -Date)

  course2_df <- read_excel(course2) %>% clean_names() %>% select(id_number, attempt, started_on) %>% rename(ID = id_number) %>%
    transform(attempt = as.numeric(attempt), ID = as.numeric(ID)) %>% na.omit %>%  filter(attempt==1) %>%
    separate(started_on, into = c("Date", "Time"), sep = ",") %>% transform(Date =as.Date(Date, "%d %B %Y")) %>%
    filter(Date <= as.Date(date)) %>% select(-Time, -Date)

  combined1 <- left_join (staff_df, course1_df) %>% replace_na(list(attempt = 0))
  combined2 <- left_join (staff_df, course2_df) %>% replace_na(list(attempt = 0))

  both <- left_join(combined1, combined2, by= "ID") %>% select(1:6,11)
  colnames(both) <- c("ID", "name", "area", "directorate", "division", "course1", "course2")
  both$both <- ifelse((both$course1 == 1 & both$course2 == 1), 1,0)

  both %>% filter(area =="Data Capability"| area =="Economic Social and Environmental"| area =="Health Population and Methods") %>%
    group_by(division) %>% summarise(course1 = sum(course1), course2 = sum(course2), both=sum(both)) %>%
    mutate(course1_prc = round(sum(course1)/n, 2), course2_prc = round(sum(course2)/n,2), both_prc=round(sum(both)/n,2))%>%
    select(1,5,2,6,3,7,4,8)
}

