library(lubridate)
library(dplyr)

#' Add column of time-shifted shifted metrics
#' 
#' This function takes a dataframe with a date column and a metric column,
#' along with any other dimensions in the dataset, and creates a new
#' metric column time-shifted by the parameters specified. E.g. clicks for
#' this row one year ago or one week ago.
#' 
#' @param data.in The dataframe containing a date and metric
#' @param metric
#' @param date.col The date column in the dataframe
#' @param time.shift The number of period shifts to apply
#' @param shift.per The period of time to shift by
#' @export

addTimeShift <- function(data.in,
                         metric,
                         date.col = "date",
                         time.shift = 1,
                         shift.per = 364) {
  
  # Convert to dataframe to prevent weird grouped_df issues
  data.in <- as.data.frame(data.in)
  
  # Defaults for other shift periods
  shift.per.2 <- shift.per * 2
  shift.per.3 <- shift.per * 3
  
  # Make sure date column exist
  if (!exists(date.col, where = data.in)) {
    stop("Date column in not found in this dataframe")
  }
  
  # Only support 1-3 shifts
  if (!time.shift %in% c(1, 2, 3)) {
    stop("addTimeShift only supports 1 to 3 shifts")
  }
  
  # Remove date from list (so we can distinguish dimensions for sort)
  data.out <- data.in
  data.out[, date.col] <- NULL
  
  # Get list of dataframe dimensions
  dimensions <- lapply(colnames(data.out[!sapply(data.out, is.numeric)]), as.symbol)
  arrange.dimensions <- c(dimensions, as.symbol(date.col))
  
  # Sort & group data frame
  data.out <- arrange_(data.in, .dots = arrange.dimensions) %>%
    group_by_(.dots = dimensions)
  
  # Add lagged columns (naming workarounds for bugs in NSE w/ dplyr & lag)
  names(data.out)[names(data.out) == metric] <- "pmetric"
  data.out <- switch(time.shift,
                     mutate(data.out, 
                            n.1 = lag(pmetric, shift.per)),
                     mutate(data.out, 
                            n.1 = lag(pmetric, shift.per),
                            n.2 = lag(pmetric, shift.per.2)),
                     mutate(data.out, 
                            n.1 = lag(pmetric, shift.per),
                            n.2 = lag(pmetric, shift.per.2),
                            n.3 = lag(pmetric, shift.per.3)))

  names(data.out)[names(data.out) == "pmetric"] <- metric
  
  return(data.out)
  
}