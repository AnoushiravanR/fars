accident_2013 <- readr::read_csv("accident_2013.csv.bz2")
accident_2014 <- readr::read_csv("accident_2014.csv.bz2")
accident_2015 <- readr::read_csv("accident_2015.csv.bz2")

#' American public yearly Data regarding fatal injuries suffered in motor vehicle
#'
#' Data from US National Highway Traffic Safety Administration's Fatality
#' Analysis Reporting System
#'
#' @format A Data frame
#' \describe{
#'  \item{STATE}{A numeric value}
#'  \item{ST_CASE}{A numeric value}
#'  \item{VE_TOTAL}{A numeric value}
#'  \item{VE_FORMS}{A numeric value}
#' ...
#' }
#'
#' @source \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
"accident_2013"
"accident_2014"
"accident_2015"
