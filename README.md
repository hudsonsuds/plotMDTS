# plotMDTS
A wrapper around dplyr and ggplot2 for simple/fast plotting and analysis of a multi-dimensional Time Series

## Description
Plotting a multi-dimensional dataset repeatedly or interactively can be a pain in ggplot or R. Before you can get to any visualizing, you first have to aggregate or massage your data in just the right way before you can see what it looks like. And more often than not, the process you'll go through is the same:

1. Identify the date and metric columns
2. Choose which dimensions you want to keep (Geo, device, etc.)
3. Aggregate the data frame across the dimensions you don't want to keep (user, product, etc.)
4. Plot the summarized dataset

With plotMDTS, you can do all 4 steps with a single plotting function.

## Installation
You can use devtools and install from this github repo:
```
library(devtools)

install_github("hudsonsuds/plottMDTS")
```

## Examples
I couldn't find a good dataset to use for demos, so dropped one at:
https://docs.google.com/spreadsheets/d/11Jkv-wkYVguoAPnogB-5MAjG9qjJ-SUIpt0Ic9uPpU8/edit#gid=0
```
read.csv("~/Downloads/Fake MDTS Data - Data.csv")
example.data <- read.csv("~/Downloads/Fake MDTS Data - Data.csv")
example.data$date <- as.Date(example.data$date)
```

And now we can jump into plotting! First we can take a look at Impressions over time:
```
plotMDTS(example.data, metric = "Impressions")
```

Or zoom in on just the last year: 
```
plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-04-01")
```

Perhaps we want to look at this data by channel:
```
plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-04-01", 
         group = "Channel")
```

That was pretty noisy though (as the data is random). So let's facet it instead:
```
plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-04-01", 
         facet = "Channel")
```

Sometimes I find it useful to look at a narrow region of time, so want to know when data point occur on the weekend:
```
plotMDTS(example.data, metric = "Impressions", filter = ~date > "2014-01-01" & date < "2014-02-01", 
         weekends = TRUE)
```
