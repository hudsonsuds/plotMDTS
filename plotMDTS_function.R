require(ggplot2)
require(scales)
require(dplyr)
require(lazyeval)
require(stringr)

#' Plot a Multi-Dimensional Time-Series
#' 
#' This wrapper enables you to quickly and powerfully plot a multi-dimensional
#' time-series dataset using ggplot2 and dplyr. 
#' @param data.in A time-series data frame with a column of dates named "date"
#' @param metric The metric you want to plot (required)
#' @export
#' @examples plotMDTS(example_data, metric="raisedAmt", filters = ~date > '2007-01-01',
#'                    group = "category", facet = "state")
#' 

plotMDTS <- function(
  data.in,
  metric = NULL,
  filters = NULL,
  group = NULL,
  facet = NULL,
  by.date = "Daily",
  title = metric,
  ylab = metric,
  xlab = "Date",
  weekends = FALSE,
  add.zeros = FALSE,
  counting = FALSE,
  y.zero = FALSE
) {
  
  # Convert data.in into a data frame (so group operaitons work)
  data.in <- as.data.frame(data.in)
  
  # Check for count of metric
  if(grepl("n\\(.*\\)", metric)) {
    counting <- TRUE
    metric <- substr(metric, 3, nchar(metric) - 1)
  }
  
  # Verify columns 
  stopifnot(exists("date", where = data.in))
  stopifnot(exists(metric, where = data.in))  
  
  data.clean <- data.in
  
  # Filter data
  if (!is.null(filters)) {
    data.clean <- filter_(data.clean, filters)
    
    # Drop NAs for defined metric
    data.clean <- filter(data.clean, !is.na(interp(metric)))
  }  
  
  # TODO(mbh): Add support for data aggregation; use lubridate?
  #            Looks like this might also solve the time support too.
  
  # Aggregate across dimensions except group and facet
  if (!is.null(group)){
    if (!is.null(facet)) {
      # Both group and facet
      data.group <- group_by_(data.clean, "date", group, facet)
      
    } else {
      # Just group
      data.group <- group_by_(data.clean, "date", group)
      
    }
  } else if (!is.null(facet)) {
    # Just facet
    data.group <- group_by_(data.clean, "date", facet)
    
  } else {
    # Neither
    data.group <- group_by(data.clean, date)
    
  }
  
  # Add metric
  if(counting) {
    data.group <- summarize_(data.group,
                            value = interp(~n_distinct(var), var = as.name(metric)))
    
  } else {
    data.group <- summarize_(data.group,
                             value = interp(~sum(var, na.rm = TRUE), 
                                            var = as.name(metric)))
  }
  
  # Rename value to metric
  if(y.zero) {
    min.val <- min(0, min(data.group$value, na.rm = TRUE))
  } else {
    min.val <- min(data.group$value, na.rm = TRUE)
  }
  max.val <- max(data.group$value, na.rm = TRUE)
  colnames(data.group)[colnames(data.group)=="value"] <- metric
  
  # Fill in zeros for missing values
  if(add.zeros) {
    data.group <- addMissingZeros(data.group)
    
  }
  
  # Create plot
  data.plot <- ggplot()
  
  # Add weekends
  if(weekends) {
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
                fill = "grey80", alpha = "0.15")
  }
  
  # Lines to plot
  if(!is.null(group)) {
    # Adding group color
    data.plot <- data.plot + 
      geom_line(data = data.group, aes_string(x = "date", y = metric, color = group)) +
      geom_point(data = data.group, aes_string(x = "date", y = metric, color = group))
    
    
  } else {
    # No group
    data.plot <- data.plot + 
      geom_line(data = data.group, aes_string(x = "date", y = metric)) +
      geom_point(data = data.group, aes_string(x = "date", y = metric))
    
  }
  
  # Add facets
  if(!is.null(facet)) {
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
