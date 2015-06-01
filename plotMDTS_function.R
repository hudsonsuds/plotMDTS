require(ggplot2)
require(scales)
require(dplyr)
require(assertthat)

#' Plot a Multi-Dimensional Time-Series
#' 
#' This wrapper enables you to quickly and powerfully plot a multi-dimensional
#' time-series dataset using ggplot2 and dplyr. 
#' @param data.in A time-series data frame with a column of dates named "date"
#' @param metric The metric you want to plot (required)
#' @export
#' 

plotMDTS <- function(
  data.in,
  metric = NULL,
  filters = NULL,
  group = NULL,
  facet = NULL,
  by.date = "Daily",
  title = metric,
  y_axis = metric,
  x_axis = "Date"
) {
  
  # Verify key columns 
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
  data.group <- summarize_(data.group,
                           value = paste0("sum(", metric,", na.rm=TRUE)"))
  
  # Rename value to metric
  colnames(data.group)[colnames(data.group)=="value"] <- metric
  
  # TODO(mbh): Fill zeros for segments missing data
  # TODO(mbh): Add support for weekend highlighting
  
  # Create plot
  data.plot <- ggplot() +     
    ggtitle(title) +
    scale_y_continuous(breaks = pretty_breaks()) +
    ylab(y_axis) + 
    xlab(x_axis) + 
    theme_bw() + 
    theme(legend.title=element_blank()) +
    scale_colour_brewer(palette="Set1")
  
  # Lines to plot
  if(!is.null(group)) {
    # Adding group color
    data.plot <- data.plot + geom_line(data = data.group, aes_string(x = "date", y = metric, color = group))
    
  } else {
    # No group
    data.plot <- data.plot + geom_line(data = data.group, aes_string(x = "date", y = metric))
    
  }
  
  # Add facets
  if(!is.null(facet)) {
    # Add facet
    data.plot <- data.plot + facet_wrap(as.formula(paste0("~", facet)))
    
  }
  
  # Return plot
  return(data.plot)
}
