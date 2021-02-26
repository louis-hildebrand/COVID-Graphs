# COVID-Graphs

This R script plots the latest COVID-19 data for Canada using the ggplot library.

All data is obtained from the Government of Canada's website: https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html#a1

## Example plots
#### Canada (updated Feb. 25, 2021)
![](https://github.com/louis-hildebrand/COVID-Graphs/blob/master/Example%20graphs/Canada%207-day%20daily%20cases%20(2021-02-25).png)

#### Quebec (updated Feb. 25, 2021)
![](https://github.com/louis-hildebrand/COVID-Graphs/blob/master/Example%20graphs/Quebec%207-day%20daily%20cases%20(2021-02-25).png)

## Using the script
The following libraries are required:
* ggplot2:    https://ggplot2.tidyverse.org/
* reshape2:   https://www.rdocumentation.org/packages/reshape2
* stringr:    https://www.rdocumentation.org/packages/stringr

*** Important ***
In src/main.r, you must change the value of home_dir to point to the root of the project directory.

Two functions are provided for producing the plots. Call them at the very end of main.r.
```
plot_cumulative(type, region = "CANADA", period = c(as.POSIXct("2020-03-11"), RAW_DATA[nrow(RAW_DATA), "date"]), logScale = F, annotations = NULL)
    Plots the cumulative confirmed+probable cases and/or deaths

    type:         a character vector with "cases", "deaths", "active", and/or "recovered"
    region:       province name or "CANADA"
    period:       vector with the desired starting and ending dates (inclusive, formatted as POSIXct) or blank for no restriction
    logScale:     if TRUE, the graph's y-axis will be on a log scale
    annotations:  optional data frame of annotations to add to the plot. Examples of the format are given in annotations.r.


plot_new(type, region = "CANADA", period = c(RAW_DATA[1, "date"], RAW_DATA[nrow(RAW_DATA), "date"]), avgLen = 1, annotations = NULL, correct = F)
    Plots average daily confirmed+probable cases and/or deaths

    type:         a character vector with "cases," "deaths," "active," and/or "recovered"
    region:       province name or "CANADA"
    period:       vector with the desired starting and ending dates (inclusive, formatted as POSIXct) or blank for no restriction
    avgLen:       number of days over which to calculate the running average
    annotations:  optional data frame of annotations to add to the plot. Examples of the format are given in annotations.r.
    correct:      if TRUE, Quebec's 1317 extra cases on May 3 will be distribted from April 2 to 30 (proportionally to the recorded number of new cases in Quebec on each day)
```
