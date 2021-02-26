# Plots average daily confirmed+probable cases and/or deaths
# type: "cases", "deaths", or c("cases", "deaths")
# region: province name or "CANADA"
# period: vector with the desired starting and ending dates (inclusive, formatted as yyyy-mm-dd) or blank for no restriction
# avgLen: number of days over which to calculate the running average
# annotate: if TRUE, the graph will be overlayed with important government policies
# correct: if TRUE, Quebec's 1317 extra cases on May 3 will be distribted from April 2 to 30 (proportionally to the recorded number of new cases in Quebec on each day)
plot_new <- function(type, region = "CANADA", period = c(RAW_DATA[1, "date"], RAW_DATA[nrow(RAW_DATA), "date"]), avgLen = 1, annotate = NULL, correct = F)
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
