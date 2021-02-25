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
