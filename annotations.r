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