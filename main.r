setwd("c:/users/louis/google drive/personal projects/covid data")

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
source("data-preparation.r")

# Define utility functions and plotting functions
source("utility.r")
source("cumulative-plot.r")
source("new-plot.r")

# Define annotations (old, only for Quebec)
source("annotations.r")





# source("c:/users/louis/google drive/personal projects/covid data/main.r", echo=T)
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#cumulative(type = c("cases", "active"), region = "Quebec")
new(type = c("cases", "deaths"), region = "Quebec", avgLen = 7)

