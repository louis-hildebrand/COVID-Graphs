library(ggplot2)
library(reshape2)
library(stringr)

options(scipen = 999, tz = "EST")		# Pevent the use of scientific notation (e.g. in log scale plots) and set time zone
COL_CASES <- "darkblue"		# Colour for total/daily cases
COL_ACTIVE <- "deepskyblue"	# Colour for active cases
COL_DEATHS <- "red2"		# Colour for deaths
COL_RECOVER <- "green2"		# Colour for recovered patients
getNew <- T					# Should it check for new data on the Government website
DATE_FORMAT <- "%d-%m-%Y"

# Prepare data
if (!file.exists("C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv")) {
	print("Data not found: downloading latest data from government website")
	download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", "C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv")
	RAW_DATA <- read.csv("C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv", header = T)
} else {
	RAW_DATA <- read.csv("C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv", header = T)
	# If the last date in the data frame is less than the current date, go download the latest data from the government website
	mostRecentDate = as.Date(RAW_DATA[nrow(RAW_DATA), "date"], format = DATE_FORMAT)
	if (!getNew) {
		print("getNew is FALSE: did not check for newest data")
	} else if (Sys.Date() - mostRecentDate >= 2) {
		print("Data is out of date: downloading latest data from government website")
		download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", "C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv")
		RAW_DATA <- read.csv("C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv", header = T)
	} else if (Sys.Date() - mostRecentDate == 1 && Sys.time() > as.POSIXct("19:00:00", format = "%k:%M:%S")) {
		print("Data is out of date: downloading latest data from government website")
		download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", "C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv")
		RAW_DATA <- read.csv("C:/Users/louis/Google Drive/Personal Projects/COVID Data/covid19.csv", header = T)
	} else {
		print("Data is up to date!")
	}
}
Sys.sleep(2)
RAW_DATA$date <- as.POSIXct(RAW_DATA$date, format = DATE_FORMAT)
RAW_DATA$prname <- toupper(RAW_DATA$prname)

# Returns the date that is _days_ days before currentDate
daysBefore <- function(currentDate, days)
{
	pastDate <- currentDate - 60*60*24*days
	
	# Round to the nearest day to avoid problems with daylight savings time
	pastDate <- round(pastDate, "days")
	
	return(pastDate)
}

# Returns a vector with the running average of the values in df
# df: column one contains dates, column 2 contains cumulative cases or deaths
# avgLen: number of days over which to average
runAvg <- function(df,  avgLen)
{
	avg = vector()
	for (i in 1:nrow(df))
	{
		startDate = daysBefore(df[i, 1], avgLen)
		if (nrow(df[df[,1] == startDate,]) == 1)
		{
			avg <- c(avg, (df[df[,1] == df[i, 1], 2] - df[df[,1] == startDate, 2])/avgLen)
		}
		else
		{
			avg <- c(avg, NA)
		}
	}
	return(avg)
}

# Adds annotations to plot and returns the plot
# graph: the ggplot to which to add notes
# annotate: a data frame with annotations. Col 1 is the text, col 2 is a POSIXct date, col 3 is a y-coordinate, and col 4 is a bool (TRUE if the coordinates are for the top left corner of the text box, FALSE if top right)
addAnnotations <- function(graph, annotate)
{
	graph <- graph + annotate("segment", linetype = "longdash", x = annotate[, 2], xend = annotate[, 2], y = rep(0, nrow(annotate)), yend = annotate[, 3])
	graph <- graph + annotate("text", hjust = as.integer(annotate[, 4]), vjust = 1, label = annotate[, 1], x = annotate[, 2], y = annotate[, 3])
	
	return(graph)
}

# Plots the cumulative confirmed+probable cases and/or deaths
# type: "cases", "deaths", "active", and/or "recovered"
# region: province name or "CANADA"
# period: vector with the desired starting and ending dates (inclusive, formatted as yyyy-mm-dd) or blank for no restriction
# logScale: if TRUE, the graph's y-axis will be on a log scale
# annotate: if TRUE, the graph will be overlayed with important government policies
cumulative <- function(type, region = "CANADA", period = c(as.POSIXct("2020-03-11"), RAW_DATA[nrow(RAW_DATA), "date"]), logScale = F, annotate = NULL)
{
	# Input validation
		# type
	type <- toupper(type)
	tempType <- type
	for (t in c("CASES", "DEATHS", "ACTIVE", "RECOVERED"))
	{
		if (length(type[type == t]) > 1)
		{
			stop(paste0(t, " appears more than once in _type_"))
		}
		tempType <- tempType[tempType != t]
	}
	if (length(tempType) > 0)
	{
		stop("Invalid type(s). The valid types are: \"cases\", \"deaths\", \"active\", \"recovered\"")
	}
		# region
	region = toupper(region)
	if (length(region) != 1)
	{
		stop("You must choose exactly one region.")
	}
	if (!(region %in% levels(factor(RAW_DATA$prname))))
	{
		stop(paste0(region, " is not a valid region. The valid regions are: ", paste(levels(factor(RAW_DATA$prname)), collapse = ", ")))
	}
		# period
	if (length(period) != 2)
	{
		stop("_period_ must have exactly two elements")
	}
	if (!inherits(period, "POSIXct"))
	{
		stop("_period_ must contain only POSIXct dates")
	}
		# logScale
	if (length(logScale) != 1)
	{
		stop("_logScale_ must have exactly one element")
	}
	if (class(logScale) != "logical")
	{
		stop(paste0("_logScale_ must be logical. Current class is ", class(annotate)))
	}
		# annotate
	if (!is.null(annotate))
	{
		if (class(annotate) != "data.frame" || ncol(annotate) != 4)
		{
			stop("_annotate_ must be a data frame with exactly 4 columns")
		}
		if (!inherits(annotate[, 2], "POSIXct"))
		{
			stop("Column 2 in _annotate_ must contain only POSIXct dates")
		}
		if (!all(is.numeric(annotate[, 3])))
		{
			stop("Column 3 in _annotate_ must contain only numeric y-coordinates")
		}
		if (!all(is.logical(annotate[, 4])))
		{
			stop("Column 4 in _annotate_ must contain only boolean values")
		}
	}
	
	# Get relevant data from RAW_DATA
	columns <- "date"
	column_names <- "date"
	colours <- vector()
	if ("CASES" %in% type)
	{
		columns <- c(columns, "numtotal")
		column_names <- c(column_names, "Total Cases")
		colours <- c(colours, COL_CASES)
	}
	if ("ACTIVE" %in% type)
	{
		columns <- c(columns, "numactive")
		column_names <- c(column_names, "Active")
		colours <- c(colours, COL_ACTIVE)
	}
	if ("RECOVERED" %in% type)
	{
		columns <- c(columns, "numrecover")
		column_names <- c(column_names, "Recovered")
		colours <- c(colours, COL_RECOVER)
	}
	if ("DEATHS" %in% type)
	{
		columns <- c(columns, "numdeaths")
		column_names <- c(column_names, "Deaths")
		colours <- c(colours, COL_DEATHS)
	}
	dat <- RAW_DATA[RAW_DATA$date >= period[1] & RAW_DATA$date <= period[2] & RAW_DATA$prname == region, columns]
	colnames(dat) <- column_names
	
	print(dat)
	
		# Make the total number of cases go behind all the other stats by subtracting active, recovered and/or deaths and then stacking the remaining cases on top
	dat[is.na(dat)] <- 0
	if ("CASES" %in% type && length(type) > 1)
	{
		for (c in 3:ncol(dat))
		{
			dat[, "Total Cases"] <- dat[, "Total Cases"] - dat[, c]
		}
		if (all(dat[, "Total Cases"] == 0))
		{
			dat <- dat[, -2]
			colours <- colours[colours != COL_CASES]
		}
	}
	if (logScale)
	{
		dat[dat < 1] <- NA
	}
	
	# Convert data to long form
	dat <- melt(dat, id.vars = "date")
	
	# Set up ggplot and add basic aesthetics
	graph <- ggplot(dat, aes(x = date, y = value)) + 
		geom_bar(stat = "identity", position = "stack", aes(fill = variable, color = variable)) +
		scale_colour_manual(values = colours) +
		scale_fill_manual(values = colours) +
		scale_x_datetime(date_labels = "%b %d") +
		labs(fill = NULL) +
		guides(color = F) +
		theme(legend.position = c(0.05, 0.95), legend.justification = c("left", "top"))
	
	# Titles
	title <- paste0("Cumulative COVID-19 Data in ", str_to_title(region))
	if (("CASES" %in% type || "ACTIVE" %in% type || "RECOVERED" %in% type) && "DEATHS" %in% type) { ylab <- "Cumulative cases & deaths" }
	else if ("CASES" %in% type || "ACTIVE" %in% type || "RECOVERED" %in% type) { ylab <- "Cumulative cases" }
	else if ("DEATHS" %in% type) { ylab <- "Cumulative deaths" }
	
	if (logScale)
	{
		title = paste0(title, " (Log Scale)")
	}
	xlab = "Date"
	if (logScale)
	{
		graph <- graph + scale_y_log10()
	}
	graph <- graph + ggtitle(title, subtitle = "Source: Government of Canada, \"Coronavirus disease (COVID-19): Outbreak update\"") + xlab("Date") + ylab(ylab)
	graph <- graph +
		theme(
			plot.title = element_text(size = 16, face = "bold"),
			plot.subtitle = element_text(size = 12, face = "italic")
		)
	
	if (!is.null(annotate))
	{
		graph <- addAnnotations(graph, annotate)
	}
	
	print(graph)
	
	# Latest data:
	if ("DEATHS" %in% type)
	{
		allDeaths <- dat[dat$variable == "Deaths", "value"]
		print(paste0("Total deaths: ", allDeaths[length(allDeaths)]))
	}
	if ("RECOVERED" %in% type)
	{
		recoveredCases <- dat[dat$variable == "Recovered", "value"]
		print(paste0("Total recovered: ", recoveredCases[length(recoveredCases)]))
	}
	if ("ACTIVE" %in% type)
	{
		activeCases <- dat[dat$variable == "Active", "value"]
		print(paste0("Current active cases: ", activeCases[length(activeCases)]))
	}
	if ("CASES" %in% type)
	{
		allCases <- dat[dat$variable == "Total Cases", "value"]
		sum <- allCases[length(allCases)]
		if ("DEATHS" %in% type) { sum <- sum + allDeaths[length(allDeaths)] }
		if ("RECOVERED" %in% type) { sum <- sum + recoveredCases[length(recoveredCases)] }
		if ("ACTIVE" %in% type) { sum <- sum + activeCases[length(activeCases)] }
		print(paste0("Total confirmed cases: ", sum))
	}
	if ("DEATHS" %in% type && "RECOVERED" %in% type && "ACTIVE" %in% type)
	{
		allCases <- allDeaths[length(allDeaths)] + recoveredCases[length(recoveredCases)] + activeCases[length(activeCases)]
		print(paste0("Total confirmed cases: ", allCases))
	}
}

# Plots average daily confirmed+probable cases and/or deaths
# type: "cases", "deaths", or c("cases", "deaths")
# region: province name or "CANADA"
# period: vector with the desired starting and ending dates (inclusive, formatted as yyyy-mm-dd) or blank for no restriction
# avgLen: number of days over which to calculate the running average
# annotate: if TRUE, the graph will be overlayed with important government policies
# correct: if TRUE, Quebec's 1317 extra cases on May 3 will be distribted from April 2 to 30 (proportionally to the recorded number of new cases in Quebec on each day)
new <- function(type, region = "CANADA", period = c(RAW_DATA[1, "date"], RAW_DATA[nrow(RAW_DATA), "date"]), avgLen = 1, annotate = NULL, correct = F)
{
	# Input validation
		# type
	type <- toupper(type)
	tempType <- type
	for (t in c("CASES", "DEATHS", "ACTIVE", "RECOVERED"))
	{
		if (length(type[type == t]) > 1)
		{
			stop(paste0(t, " appears more than once in _type_"))
		}
		tempType <- tempType[tempType != t]
	}
	if (length(tempType) > 0)
	{
		stop("Invalid type(s). The valid types are: \"cases\", \"deaths\", \"active\", \"recovered\"")
	}
		# region
	region = toupper(region)
	if (length(region) != 1)
	{
		stop("You must choose exactly one region.")
	}
	if (!(region %in% levels(factor(RAW_DATA$prname))))
	{
		stop(paste0(region, " is not a valid region. The valid regions are: ", paste(levels(factor(RAW_DATA$prname)), sep = ", ")))
	}
		# period
	if (length(period) != 2)
	{
		stop("_period_ must have exactly two elements")
	}
	if (!inherits(period, "POSIXct"))
	{
		stop("_period_ must contain only POSIXct dates")
	}
		# avgLen
	if (length(avgLen) != 1)
	{
		stop("_avgLen_ must contain exactly 1 element")
	}
	if (avgLen %% 1 != 0 || avgLen <= 0)
	{
		stop("_avgLen_ must be an integer greater than zero")
	}
		# annotate
	if (!is.null(annotate))
	{
		if (class(annotate) != "data.frame" || ncol(annotate) != 4)
		{
			stop("_annotate_ must be a data frame with exactly 4 columns")
		}
		if (!inherits(annotate[, 2], "POSIXct"))
		{
			stop("Column 2 in _annotate_ must contain only POSIXct dates")
		}
		if (!all(is.numeric(annotate[, 3])))
		{
			stop("Column 3 in _annotate_ must contain only numeric y-coordinates")
		}
		if (!all(is.logical(annotate[, 4])))
		{
			stop("Column 4 in _annotate_ must contain only boolean values")
		}
	}
		# correct
	if (length(correct) != 1)
	{
		stop("_correct_ must have exactly one element")
	}
	if (class(correct) != "logical")
	{
		stop(paste0("_correct_ must be logical. Current class is ", class(annotate)))
	}
	if (correct && !("CASES" %in% type))
	{
		stop("You cannot correct the case numbers in charts that do not show the number of cases.")
	}
	if (correct && region != "QUEBEC")
	{
		stop("The _correct_ argument is only applicable for the province of Quebec")
	}
	
	# Get relevant data from RAW_DATA
	columns <- "date"
	if ("CASES" %in% type) { columns <- c(columns, "numtotal") }
	if ("DEATHS" %in% type) { columns <- c(columns, "numdeaths") }
	
	pr_dat <- RAW_DATA[RAW_DATA$prname == region, columns]
	# Re-distribute the 1317 cases on May 3 from April 2 to 30
	if (correct)
	{
		dailyCases <- runAvg(pr_dat[, c("date", "numtotal")], 1)[pr_dat$date >= as.POSIXct("2020-04-02", format = "%Y-%m-%d") & pr_dat$date <= as.POSIXct("2020-04-30", format = "%Y-%m-%d")]
		extraCases <- 1317/sum(dailyCases, na.rm = T) * dailyCases
		dayList <- seq(from = as.POSIXct("2020-04-02", format = "%Y-%m-%d"), to = as.POSIXct("2020-05-02", format = "%Y-%m-%d"), by = "1 day")
		daysToAdjust <- seq(from = as.POSIXct("2020-04-02", format = "%Y-%m-%d"), to = as.POSIXct("2020-05-02", format = "%Y-%m-%d"), by = "1 day")
		for (i in 1:length(daysToAdjust))
		{
			day <- daysToAdjust[i]
			pr_dat[pr_dat$date == day, "numtotal"] <- pr_dat[pr_dat$date == day, "numtotal"] + sum(extraCases[1:(1 + difftime(day, as.POSIXct("2020-04-02", format = "%Y-%m-%d"), units = "days"))], na.rm = T)
		}
	}
	if ("CASES" %in% type) { pr_dat <- cbind(pr_dat, Cases = runAvg(pr_dat[, c("date", "numtotal")], avgLen)) }
	if ("DEATHS" %in% type) { pr_dat <- cbind(pr_dat, Deaths = runAvg(pr_dat[, c("date", "numdeaths")], avgLen)) }
	dat <- pr_dat[pr_dat$date >= period[1] & pr_dat$date <= period[2],]
	
	# Show raw data
	print(dat)
	
	# If applicable, subtract deaths from cases so that the number of cases appears behind the number of deaths
	if ("CASES" %in% type && "DEATHS" %in% type)
	{
		dat$Cases <- dat$Cases - dat$Deaths
	}
	
	# Convert data to long form
	columns <- "date"
	colours <- vector()
	if ("CASES" %in% type) {
		columns <- c(columns, "Cases")
		colours <- c(colours, COL_CASES)
	}
	if ("DEATHS" %in% type)
	{
		columns <- c(columns, "Deaths")
		colours <- c(colours, COL_DEATHS)
	}
	dat <- melt(dat[, columns], id.vars = "date")
		# If value is 0, leave it blank
	dat$value[dat$value == 0] <- NA
	
	# Create ggplot and add cases and/or deaths as appropriate
	graph <- ggplot(dat, aes(x = date, y = value)) + 
		geom_bar(stat = "identity", aes(fill = variable, color = variable)) +
		scale_x_datetime(date_labels = "%b %d") +
		scale_colour_manual(values = colours) +
		scale_fill_manual(values = colours) +
		labs(fill = NULL) +
		guides(color = F)
	
	# If there is only one data type being displayed, remove the legend
	if (length(type) == 1)
	{
		graph <- graph + guides(fill = F)
	}
	
	# Titles
	title = "Daily COVID-19 "
	if (avgLen >= 2) { ylab = "Average daily " } else {ylab = "Daily "}
	if ("CASES" %in% type && "DEATHS" %in% type) 
	{
		title <- paste0(title, "Cases & Deaths ")
		ylab <- paste0(ylab, "cases & deaths")
	}
	else if ("CASES" %in% type) 
	{
		title <- paste0(title, "Cases ")
		ylab <- paste0(ylab, "cases")
	}
	else if ("DEATHS" %in% type) 
	{
		title <- paste0(title, "Deaths ")
		ylab <- paste0(ylab, "deaths")
	}
	title <- paste0(title, "in ", str_to_title(region))
	if (avgLen > 1)
	{
		title <- paste0(title, " (", avgLen, "-Day Running Average)")
	}
	subtitle <- "Source: Government of Canada, \"Coronavirus disease (COVID-19): Outbreak update\""
	if (correct)
	{
		subtitle <- paste0(subtitle, "\nNote: 1317 extra cases were reported on May 3 due to a technical error.\n           Here, those cases are redistributed from April 2 to April 30, proportionally to the official daily cases on those dates.")
	}
	graph <- graph + ggtitle(title, subtitle) + xlab("Date") + ylab(ylab)
	graph <- graph +
		theme(
			plot.title = element_text(size = 16, face = "bold"),
			plot.subtitle = element_text(size = 12, face = "italic")
		)
	
	if (!is.null(annotate))
	{
		graph <- addAnnotations(graph, annotate)
	}
	
	graph
}



# Annotations
ANN_QC_1 <- data.frame(
	text = c(" Indoor gatherings \n > 250 people banned ", " Schools closed ", " All public gatherings \n banned ", " Private gatherings: 10 people \n (except MTL) ", " Public gatherings: 50 people \n Private gatherings: 10 people ", " Mask mandatory \n in enclosed \n public spaces ", " Public gatherings: 250 people "),
	date = as.POSIXct(c("2020-03-12", "2020-03-13", "2020-03-21", "2020-06-15", "2020-06-22", "2020-07-18", "2020-08-03")),
	height = c(2000, 1700, 1400, 2000, 1700, 1100, 1400),
	topRight = c(F, F, F, F, F, T, T),
	stringsAsFactors = F)
ANN_QC_7 <- data.frame(
	text = c(" Indoor gatherings \n > 250 people banned ", " Schools closed ", " All public gatherings \n banned ", " Private gatherings: 10 people \n (except MTL) ", " Public gatherings: 50 people \n Private gatherings: 10 people ", " Mask mandatory \n in enclosed \n public spaces ", " Public gatherings: 250 people "),
	date = as.POSIXct(c("2020-03-12", "2020-03-13", "2020-03-21", "2020-06-15", "2020-06-22", "2020-07-18", "2020-08-03")),
	height = c(1200, 1025, 900, 1000, 800, 400, 600),
	topRight = c(F, F, F, F, F, T, T),
	stringsAsFactors = F)





# source("c:/users/louis/google drive/personal projects/covid data/data analysis.r", echo=T)
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
print("Hello there!")
#cumulative(type = c("cases", "active"), region = "Quebec")
new(type = c("cases", "deaths"), region = "Ontario", avgLen = 1)

