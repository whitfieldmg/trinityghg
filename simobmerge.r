# Function to create date series and merge to measured data

simobmerge = function (from, to, leap = FALSE, obs) {
	
	obsclass = sapply(obs, class)
	
	# Check that observed data has DATE column
	if (!("Date" %in% obsclass)) stop("Column of class 'Date' missing from 'obs'")
	
	# If date column exists, get its name
	datecol = names(which(sapply(obs, class) == "Date"))
	
	SERIES = seq(as.Date(from), as.Date(to), "day")
	
	# Create new dataframe with date column matching the name of that in obs
	if (leap) {
		# Don't filter out 29th February (model does handle leap years)
		daily = data.frame(SERIES)
		colnames(daily) = datecol
		} else {
		# Do filter out 29th February (model doesn't handle leap years)
		daily = data.frame(SERIES[format(SERIES, "%d %m") != "29 02"])
		colnames(daily) = datecol
	}
	
	# Merge observed data with simulation template on date column
	sim.obs = merge(daily, obs, datecol, all = T)
	
	# Return merged timeseries
	return(sim.obs)
}
