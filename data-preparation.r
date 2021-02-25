if (!file.exists("covid19.csv")) {
	print("Data not found: downloading latest data from government website")
	download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", "covid19.csv")
	RAW_DATA <- read.csv("covid19.csv", header = T)
} else {
	RAW_DATA <- read.csv("covid19.csv", header = T)
	# If the last date in the data frame is less than the current date, go download the latest data from the government website
	mostRecentDate = as.Date(RAW_DATA[nrow(RAW_DATA), "date"], format = DATE_FORMAT)
	if (!getNew) {
		print("getNew is FALSE: did not check for newest data")
	} else if (Sys.Date() - mostRecentDate >= 2) {
		print("Data is out of date: downloading latest data from government website")
		download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", "covid19.csv")
		RAW_DATA <- read.csv("covid19.csv", header = T)
	} else if (Sys.Date() - mostRecentDate == 1 && Sys.time() > as.POSIXct("19:00:00", format = "%k:%M:%S")) {
		print("Data is out of date: downloading latest data from government website")
		download.file("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv", "covid19.csv")
		RAW_DATA <- read.csv("covid19.csv", header = T)
	} else {
		print("Data is up to date!")
	}
}
Sys.sleep(2)
RAW_DATA$date <- as.POSIXct(RAW_DATA$date, format = DATE_FORMAT)
RAW_DATA$prname <- toupper(RAW_DATA$prname)