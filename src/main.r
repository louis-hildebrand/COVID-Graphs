home_dir <- "c:/users/louis/google drive/personal projects/covid data"
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





# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# plot_cumulative(type = c("cases", "active"), region = "Quebec")
plot_new(type = c("cases", "deaths"), region = "Quebec", avgLen = 7)

