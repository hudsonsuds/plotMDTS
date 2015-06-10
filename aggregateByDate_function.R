require(lubridate)
require(dplyr)

#' Aggregate a dataframe by date dimensions
#' 
#' This function takes a dataframe with a date dimension and aggregates
#' it based on common date conventions -- daily, weekly, monthly, quarterly,
#' yearly. It will return a dataframe with the same dimensionality and column
#' names, but the "date" column will be aggregated with all the values of the 
#' period on the first date of that period (e.g. yearly will return as yyyy-01-01)
#'
#' @param data.in A dataframe with a date column (and other metrics or dimensions)
#' @param date.col The date column to use (defaults to "date")
#' @param by.date The granularity to summarize by: day, week, month, year
#' @export

aggregateByDate <- function(data.in,
                            date.col = "date",
                            by.date = "day") {
  
  # Make sure date column exist
  if (!exists(date.col, where = data.in)) {
    stop("Date column in not found in this dataset.")
  }
  
  # Adjust date to the specified unit (day, week, month, year)
  data.in[, date.col] <- floor_date(data.in[, date.col], by.date)
  
  # Get list of dataframe dimensions
  dimensions <- lapply(colnames(data.in[!sapply(data.in, is.numeric)]), as.symbol)
  
  # Group dataframe by date and aggregate
  data.out <- group_by_(data.in, .dots=dimensions) %>% 
    summarise_each(funs(sum))
  
  return(data.out)
  
}