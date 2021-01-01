utils::globalVariables(c("MONTH", "STATE", "year", "n"))
#'
#' @title
#' Read a delimited file into a tibble
#'
#' @description
#' This function reads a comma separated value file data.
#'
#' @param filename A character string to be supplied to function to read and it
#'  does not have default value.
#'
#' @details it first checks whether the file exists in the current working directory. If
#'  the file doesn't exist it throws an error, otherwise it reads the file and transforms
#'  it into a tibble.
#'
#' @return This function returns a tibble.
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title
#' Make file name
#'
#' @description
#' This function takes a four-digit number as its argument and returns a character
#' vector containing a formatted combination of filename text template and year variable.
#'
#' @param year A four-digit number indicating the year in which the data collected.
#'
#' @details This function takes a four-digit number as its \code{year}
#'  argument indicating the year in which the data has been collected.
#'  Then it uses \code{\link[base]{as.integer}} to coerce it into integer type.
#'  After that the function calls \code{\link[base]{sprintf}} to combine the
#'  format string for filename with the year variable. The length of the output
#'  will be equal to the longest input.
#'
#' @return This function returns a character vector containing a formatted combination
#'  of file name template in text and year variable.
#'
#' @note Because formats \code{d} can only be used for integer values or sometimes
#'  for logical values, the function use \code{\link[base]{as.integer}} to turn the
#'  entry for \code{year} argument into integer value.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title
#' Read multiple file names
#'
#' @description
#'  This function takes a vector of numeric values and read their
#'  corresponding file names into a tibble.
#'
#' @param years a numeric vector containing four-digit numeric values.
#'
#' @details \code{fars_read_years} first takes a numeric vector containing four-digit
#'  elements. It then calls \code{\link{make_filename}} to turn every elements of
#'  years vector into a character vector of formatted file names. The file
#'  names are used to read the corresponding date files into tibbles.
#'  for the purpose of error handling the function calls\code{\link[base]{tryCatch}}
#'  in order to check whether the provided file name can be evaluated and read
#'  into tibble or not. In case an invalid year name is provided an error will
#'  be thrown.
#'
#' @return A list with length equal to the length of numeric vector \code{years}
#'  in case year names are correctly provided. Elements of the list are
#'  tibbles containing 2 columns, month and year.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title
#' Summarize grouped accident data
#'
#' @description
#'  This function takes a numeric vector of four-digit name of years and
#'  creates a single tibble containing data from various years.
#'
#' @inheritParams fars_read_years
#'
#' @details This function takes a numeric vector of four-digit name of
#'  years. It calls to \code{fars_read_years} to create a list of tibbles containing data
#'  for the corresponding years. After that it calls to \code{\link[dplyr]{bind_rows}}
#'  to attach row-wise the elements of the list in order to create one single tibble.
#'  It subsequently groups the data base on year and month and summarizes the
#'  number of fatalities in every month per year.
#'
#' @return A tibble
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title
#' Make a map of accident data per state
#'
#' @description
#' This function creates a map of accident data corresponding to
#' the value of \code{year} argument. It then adds some points on the map based on
#' the coordinates provided by the dataset.
#'
#' @param state.num a numeric value of length 1
#'
#' @inheritParams make_filename
#'
#' @details This function takes two numeric values as its arguments. It first calls
#'  \code{make_filename} and \code{fars_read} to create a character vector of file name
#'  corresponding to the argument \code{year} and then reads it into a tibble.
#'  If the date for that year does not exist in the working directory the function
#'  throws an error. It then coerced the numeric value for argument \code{state.num}
#'  into integer. After that it checks whether the required \code{state.num} exists
#'  in the column STATE in the dataset. If it does not exists the function
#'  throws an error mentioning that \code{STATE} number provided is invalid.
#'  Subsequently it calls to \code{\link[dplyr]{filter}} to create a subset of the
#'  data set based on \code{state.num} value. It then counts the number of rows of
#'  the subset and if it equals to zero a message appears on the console that no
#'  accidents to plot. The function turns the values which are greater than 900 on
#'  LONGITUD column of the data set and also values which are greater than 90 on
#'  LATITUDE column into NAs. It then call to \code{\link[maps]{map}} to make
#'  a map of subset data set by means of \code{\link[base]{with}}. After that the
#'  function uses \code{\link[graphics]{points}} to add some points on the map.
#'
#' @return A list with x and y range and names components.
#'
#' @importFrom magrittr %>%
#'
#' @note if you supply an invalid \code{state.num} to this function you will
#' receive an error message "invalid STATE number". If there are no traffic
#' fatalities you will receive a message "no accident to plot".
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


