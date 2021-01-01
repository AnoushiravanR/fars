## Aims of the package

This package allows you to make a map of every state and to put pointers on it 
based on geographical positions of accidents occurred in a given year. It takes
data sets from US National Highway Traffic Safety Administration's Fatality 
Analysis Reporting System and after reading them into R, it calculates the 
number of accidents per month for every year and then generates a map of the 
given state based on state.num and year arguments.

## Installation
devtools::install_github("AnoushiravanR/fars")

## Example

library(fars)

fars_summarize_years(c(2013, 2014, 2015))

fars_map_state(48, 2015)
