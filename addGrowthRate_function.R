require(lubridate)
require(dplyr)

#' Calculate a growth rate for a metric in a dataframe
#' 
#' This function takes a dataframe with a date column and a metric column,
#' along with any other dimensions in the dataset, and creates a new
#' metric column time-shifted by the parameters specified. E.g. clicks for
#' this row one year ago or one week ago.
#' 
#' @param data.in The dataframe containing a date and metric
#' @param date The date column in the dataframe
#' @param n.per The period of time to shift by (day, week, month, year)
#' @param n.shift The number of period shifts to apply
#' @export

addGrowthRate <- function(data.in,
                          metric,
                          date.col = "date",
                          growth.per = 364) {
  
  # Convert to dataframe to prevent weird grouped_df issues
  data.in <- as.data.frame(data.in)
  
  # Make sure date column & metric exist
  if (!exists(date.col, where = data.in)) {
    stop("Date column in not found in this dataframe")
  }
  
  if (!exists(metric, where = data.in)) {
    stop(paste(metric, "not found in this dataframe"))
  }
  
  # Add a previous period column
  data.calc <- addTimeShift(data.in, metric = metric, time.shift = 1, shift.per = growth.per)
  
  # Calculate the growth rate TODO (mbh): Get rid of hack for names
  names(data.calc)[names(data.calc) == metric] <- "pmetric"
  data.calc$growth.rate <- (data.calc$pmetric / data.calc$n.1) - 1
  names(data.calc)[names(data.calc) == "pmetric"] <- metric
  
  return(data.calc)
  
}