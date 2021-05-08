home_dir <- "c:/users/louis/projects/covid data"
setwd(home_dir)

library(ggplot2)
library(reshape2)
library(stringr)

options(scipen = 999, tz = "EST")		# Pevent the use of scientific notation (e.g. in log scale plots) and set time zone
COL_CASES <- "darkblue"					# Colour for total/daily cases
COL_ACTIVE <- "deepskyblue"				# Colour for active cases
COL_DEATHS <- "red2"					# Colour for deaths
COL_RECOVER <- "green2"					# Colour for recovered patients
getNew <- T								# Should it check for new data on the Government website?
DATE_FORMAT <- "%d-%m-%Y"

# Prepare data
source("src/data-preparation.r")

# Define utility functions and plotting functions
source("src/utility.r")
source("src/plot-cumulative.r")
source("src/plot-new.r")

# Define annotations (old, only for Quebec)
source("src/annotations.r")





# USAGE
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# plot_cumulative(type, region = "CANADA", period = c(as.POSIXct("2020-03-11"), RAW_DATA[nrow(RAW_DATA), "date"]), logScale = F, annotations = NULL)
# 		Plots the cumulative confirmed+probable cases and/or deaths
#
# 		type: 			a character vector with "cases", "deaths", "active", and/or "recovered"
# 		region: 		province name or "CANADA"
# 		period: 		vector with the desired starting and ending dates (inclusive, formatted as POSIXct) or blank for no restriction
# 		logScale: 		if TRUE, the graph's y-axis will be on a log scale
# 		annotations: 	optional data frame of annotations to add to the plot. Examples of the format are given in annotations.r.
#
#
# plot_new(type, region = "CANADA", period = c(RAW_DATA[1, "date"], RAW_DATA[nrow(RAW_DATA), "date"]), avgLen = 1, annotations = NULL, correct = F)
# 		Plots average daily confirmed+probable cases and/or deaths
# 
# 		type: 			a character vector with "cases," "deaths," "active," and/or "recovered"
# 		region: 		province name or "CANADA"
# 		period: 		vector with the desired starting and ending dates (inclusive, formatted as POSIXct) or blank for no restriction
# 		avgLen: 		number of days over which to calculate the running average
# 		annotations: 	optional data frame of annotations to add to the plot. Examples of the format are given in annotations.r.
# 		correct: 		if TRUE, Quebec's 1317 extra cases on May 3 will be distribted from April 2 to 30 (proportionally to the recorded number of new cases in Quebec on each day)
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------





# plot_cumulative(type = c("cases", "active"), region = "Quebec")
plot_new(type = c("cases", "deaths"), region = "Quebec", avgLen = 7)

