library(ggplot2)
library(scales)
library(dplyr)
library(lazyeval)
library(stringr)

#' Plot a Multi-Dimensional Time-Series
#' 
#' This wrapper enables you to quickly and powerfully plot a multi-dimensional
#' time-series dataset using ggplot2 and dplyr.
#' @param data.in A time-series data frame with a column of dates named "date"
#' @param metric The metric you want to plot. Count unique values of a column with: "n(my_dimension)"
#' @param filters A formula to filter your dataset by
#' @param group The dimension you'd like to group the dataset by within a plot
#' @param facet The dimension you'd like to group the dataset by across different plots
#' @param by.date The level of date aggregation to peform (day, week, month, year)
#' @param time.shift The number of period shifts to apply
#' @param shift.per The period of time to shift by
#' @param growth.per The period of time to compare to for growth percents
#' @param title The title of the plot
#' @param ylab The y-axis label
#' @param xlab The x-axis label
#' @param y.zero Show the y-axis starting at zero
#' @param add.weekends Whether or not to compute and show weekend highlights for this time-series
#' @param add.zeros Whether or not to add zeros for every date:dimension combination
#' @export
#' @examples 
#' plotMDTS(example.data, metric = "Impressions")
#' plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-04-01")
#' plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-04-01", 
#'          group = "Channel")
#'          
#' plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-04-01", 
#'          facet = "Channel")
#' 
#' plotMDTS(example.data, metric = "Revenue", by.date = "week")
#' plotMDTS(example.data, metric = "Revenue", by.date = "month")
#' plotMDTS(example.data, metric = "Revenue", by.date = "year")
#' 
#' plotMDTS(example.data, metric = "Revenue", filter = ~date > "2015-01-01", time.shift = 1)
#' plotMDTS(example.data, metric = "Revenue", filter = ~date > "2015-01-01", time.shift = 2, shift.per = 30)
#' 
#' plotMDTS(example.data, metric = "Revenue.growth", filter =~date > "2015-01-01", growth.per = 30)

plotMDTS <- function(
  data.in,
  metric,
  filters = NULL,
  group = NULL,
  facet = NULL,
  by.date = "day",
  time.shift = NULL,
  shift.per = 364, 
  growth.per = 364,
  title = metric,
  ylab = metric,
  xlab = "Date",
  add.weekends = FALSE,
  add.zeros = FALSE,
  y.zero = FALSE
) {
  
  # Type as data.frame (dplyr grouped data.frames cause issues)
  data.in <- as.data.frame(data.in)
  
  # Check for count unique of a field as a metric
  count.unique <- FALSE
  if (grepl("n\\(.*\\)", metric)) {
    count.unique <- TRUE
    metric <- substr(metric, 3, nchar(metric) - 1)
  }
  
  # Check for growth calculation on metric
  growth <- FALSE
  if (grepl(".growth", metric)) {
    growth <- TRUE
    metric <- gsub(".growth", "", metric)
  }
  
  # Verify columns 
  if (!exists("date", where = data.in)) {
    stop("data.in must have a column named 'date'")
  }
  
  if (!exists(metric, where = data.in)) {
    stop(paste(metric, "not found in data.in"))
  }
  
  data.clean <- data.in
  
  # Aggregate across dimensions except group and facet
  if (!is.null(group) && !is.null(facet)) {
    # Both group and facet
    data.group <- group_by_(data.clean, "date", group, facet)
    
  } else if (!is.null(group)) {
    # Just group
    data.group <- group_by_(data.clean, "date", group)
    
  } else if (!is.null(facet)) {
    # Just facet
    data.group <- group_by_(data.clean, "date", facet)
    
  } else {
    # Neither
    data.group <- group_by(data.clean, date)
    
  }
  
  # Add metric
  if (count.unique) {
    data.group <- summarize_(data.group,
                             value = interp(~n_distinct(var), var = as.name(metric)))
    
  } else {
    data.group <- summarize_(data.group,
                             value = interp(~sum(var, na.rm = TRUE), 
                                            var = as.name(metric)))
  }
  
  # Rename value to metric
  if (y.zero) {
    min.val <- min(0, min(data.group$value, na.rm = TRUE))
  } else {
    min.val <- min(data.group$value, na.rm = TRUE)
  }
  max.val <- max(data.group$value, na.rm = TRUE)
  colnames(data.group)[colnames(data.group)=="value"] <- metric
  
  # Add growth calculation
  if (growth) {
    
    # Only by.date = "day" is currently supported
    if (!by.date == "day") {
      stop("Growth calculations are only supported wtih by.date = 'Day'")
    }
    
    data.group <- addGrowthRate(data.group,
                                metric = metric,
                                date.col = "date",
                                growth.per = growth.per)
    
    metric = "growth.rate"
  }
  
  # Add time shift
  if (!is.null(time.shift)) {
    
    # TODO (mbh): Figure out what to shift by depending on date aggregation?
    data.group <- addTimeShift(data.group, 
                               metric = metric,
                               time.shift = time.shift, 
                               shift.per = shift.per)
  }
  
  # Aggregate by date
  data.group <- aggregateByDate(as.data.frame(data.group), by.date = by.date)
  
  # Filter data
  if (!is.null(filters)) {
    data.group <- filter_(data.group, filters)
    
    # Drop NAs for defined metric
    data.group <- filter(data.group, !is.na(interp(metric)))
  }  
  
  # Fill in zeros for missing values
  if (add.zeros) {
    data.group <- addMissingZeros(data.group)
    
  }
  
  # Create plot
  data.plot <- ggplot()
  
  # Add weekends
  if (add.weekends) {
    # Calculate weekends for plotting     
    weekends <- select(
      filter(data.frame(date=seq(min(data.group$date, na.rm = TRUE), 
                                 max(data.group$date, na.rm = TRUE), by="day")), 
             weekdays(date) == "Saturday"), 
      date)    
    plot.weekends <- data.frame(xmin = weekends -.5,
                                xmax = weekends + 1.5,
                                ymin = min.val,
                                ymax = max.val)
    names(plot.weekends) <- c("xmin", "xmax", "ymin", "ymax")
    
    #Add weekends to plot
    data.plot <- data.plot +
      geom_rect(data=plot.weekends, aes(xmin=xmin, xmax=xmax, ymin=ymin, 
                                        ymax=ymax),
                fill = "grey80", alpha = 0.15)
  }
  
  # Lines to plot
  if (!is.null(group) && !is.null(time.shift)) {
    # Define shift lines
    n0 <- geom_line(data = data.group, aes_string(x = "date", y = metric, color = group))
    n1 <- geom_line(data = data.group, aes_string(x = "date", y = "n.1", color = group, alpha = .7))
    n2 <- geom_line(data = data.group, aes_string(x = "date", y = "n.2", color = group, alpha = .4))
    n3 <- geom_line(data = data.group, aes_string(x = "date", y = "n.3", color = group, alpha = .2))
    
    # Adding group and time.shift
    data.plot <- 
      switch(time.shift,
             data.plot + n0 + n1,
             data.plot + n0 + n1 + n2,
             data.plot + n0 + n1 + n2 + n3)
    
  } else if (!is.null(group)) {  
    # Adding group color
    data.plot <- data.plot + 
      geom_line(data = data.group, aes_string(x = "date", y = metric, color = group))
    
    
  } else if (!is.null(time.shift)) {
    # Define shift lines
    n0 <- geom_line(data = data.group, aes_string(x = "date", y = metric))
    n1 <- geom_line(data = data.group, aes_string(x = "date", y = "n.1", alpha = .7))
    n2 <- geom_line(data = data.group, aes_string(x = "date", y = "n.2", alpha = .4))
    n3 <- geom_line(data = data.group, aes_string(x = "date", y = "n.3", alpha = .2))
    
    # No group with time shift
    data.plot <- 
      switch(time.shift,
             data.plot + n0 + n1,
             data.plot + n0 + n1 + n2,
             data.plot + n0 + n1 + n2 + n3)
    
  } else {
    # No group & no time shift
    data.plot <- data.plot + 
      geom_line(data = data.group, aes_string(x = "date", y = metric))
    
  }
  
  # Add facets
  if (!is.null(facet)) {
    # Add facet
    data.plot <- data.plot + facet_wrap(as.formula(paste0("~", facet)))
    
  }  
  
  # Add styling
  data.plot <- data.plot + 
    ggtitle(title) +
    scale_y_continuous(breaks = pretty_breaks()) +
    ylab(ylab) + 
    xlab(xlab) + 
    theme_bw() + 
    theme(legend.title=element_blank())
  
  # Return plot
  return(data.plot)
}


#' Replace missing data points for time series with zeros
#' 
#' This function takes a multi-dimensional data frame and passes
#' back an identical one but with rows added for dates missing
#' values for each unique combination of dimensions in the dataframe
#' that has existing values (e.g. it won't complete for already
#' empty combinations).

addMissingZeros <- function(data.in) {  
  
  # Convert to data frame
  data.in <- as.data.frame(data.in)
  
  # Verify key columns 
  stopifnot(exists("date", where = data.in))
  
  # Remove date from data frame and convert from being grouped
  data.nodate <- as.data.frame(select(data.in, -date))
  
  # Determine unique combination of dimensions of data frame
  dimensions <- unique(data.nodate[!sapply(data.nodate, is.numeric)])
  
  # Possible dates of data frame
  possible.dates <- seq(min(data.in$date, na.rm = TRUE),
                        max(data.in$date, na.rm = TRUE),
                        by = "day")
  
  # Verify size of join (don't allow huge joins)
  resulting.size <- length(possible.dates) * length(dimensions)
  if (resulting.size > 2000000) {
    stop("Adding zeros for an enormous number of rows")
  }
  
  # Create sparse data frame of possible values
  if (nrow(dimensions) == 0 ) {
    
    # If there are no dimensions beyond the date and metric
    possible.rows <- data.frame(date = possible.dates)
    
  } else {
    possible.rows <- merge(
      data.frame(date = possible.dates),
      dimensions,
      all = TRUE)
    
  }
  
  # Merge with actual data frame
  data.out <- left_join(
    possible.rows,
    data.in)
  
  # Replace NAs w/ Zeros 
  # TODO(mbh): Fix to only do this for metric columns
  data.out[is.na(data.out)] <- 0
  
  return(data.out)
}
