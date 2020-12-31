## Aims of the package

This package allows you to make a map of every state and to put pointers on it 
based on geographical positions of accidents occurred in a given year. It takes
data sets from US National Highway Traffic Safety Administration's Fatality 
Analysis Reporting System and after reading them into R, it calculates the 
number of accidents per month for every year and then generates a map of the 
given state based on state.num and year arguments. 

## Accessing the package

This package can be loaded directly from GitHub using `devtools` package: 

```{r eval = FALSE}
library(devtools)
library("fars")
```

You can make use of functions from `dplyr`, `tidyr` and `readr` packages:

```{r eval = TRUE, message = FALSE}
library(dplyr)
library(tidyr)
library(readr)
```

## Example

As an example, here is how you can import multiple data sets corresponding to 
the years specified. For this purpose you have to use `fars_read_years` function.
This function first calls `fars::make_filename` to create a file name of a 
specified year:

```{r make_filename, message = FALSE}
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
filename <- make_filename(2013)
```

It then makes use of `fars::fars_read` to read the generatedfile names into R by
using `readr::read_csv`.

```{r fars_read, message = FALSE}
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

```

In order to be able to read multiple data sets into R, the function need to apply `fars_read` to a numeric vector `years` containing four-digit values of specified
years using `lapply`.

```{r fars_read_years}
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

fars_read_years(c(2013, 2014, 2015))
```

You can see that `fars_read_years` function takes as argument a numeric 
vector called `years` containing four-digit numeric values of years.  The 
function then selects `year` and `MONTH` columns of every imported data set. 
It creates a list length of which is equal to the length of `years` numeric 
vector. If the function cannot find a data set with the corresponding year 
it throws an error. 

To calculate the number of accidents in every month per year you can run:

```{r fars_summarize_years}
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

fars_summarize_years(c(2013, 2014, 2015))
```

This function first uses `fars_read_years` function in order to create a list 
of imported data sets of their respective years. It then binds them together 
using `dplyr::bind_rows` package and groups them based on `year` and `MONTH`
columns. In the end it summarizes the number of accident per month for every
year.

## Draw map with pointers

In order to make a map of every state with pointers indicating geographical 
positions of accidents across the state you can run the following function:

```{r fars_map_state}
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

```

This function takes as arguments `state.num` a numeric value corresponding to 
each state in `STATE` column of the data set and `year` a numeric value of 
the year. It should be noted that both `state.num` and `year` have to be single
values and this function is not vectorized. 
In order to make a map `fars_map_state` calls `maps::map` and for putting
putting pointers on it, it calls `graphics::points`.