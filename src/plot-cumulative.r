# Plots the cumulative confirmed+probable cases and/or deaths
# 
# type: 		a character vector with "cases", "deaths", "active", and/or "recovered"
# region: 		province name or "CANADA"
# period: 		vector with the desired starting and ending dates (inclusive, formatted as POSIXct) or blank for no restriction
# logScale: 	if TRUE, the graph's y-axis will be on a log scale
# annotations: 	optional data frame of annotations to add to the plot. Examples of the format are given in annotations.r.
#
plot_cumulative <- function(type, region = "CANADA", period = c(as.POSIXct("2020-03-11"), RAW_DATA[nrow(RAW_DATA), "date"]), logScale = F, annotations = NULL)
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
		stop(paste0("_logScale_ must be logical. Current class is ", class(annotations)))
	}
		# annotations
	if (!is.null(annotations))
	{
		if (class(annotations) != "data.frame" || ncol(annotations) != 4)
		{
			stop("_annotations_ must be a data frame with exactly 4 columns")
		}
		if (!inherits(annotations[, 2], "POSIXct"))
		{
			stop("Column 2 in _annotations_ must contain only POSIXct dates")
		}
		if (!all(is.numeric(annotations[, 3])))
		{
			stop("Column 3 in _annotations_ must contain only numeric y-coordinates")
		}
		if (!all(is.logical(annotations[, 4])))
		{
			stop("Column 4 in _annotations_ must contain only boolean values")
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
	
	if (!is.null(annotations))
	{
		graph <- addAnnotations(graph, annotations)
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
