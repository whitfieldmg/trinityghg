### get.cumulative
# Function to interpolate sparse measurements on a daily timestep over
# a period of interest and generate cumulative fluxes from measured and
# modelled series. The input dataframe must include a column of class Date,
# observed and modelled values.

get.cumulative = function (data, from, to, obs, obs.se, mod, method = "constant") {

	## Arguments:
	## data = data frame containing time series
	## from = date from which to start interpolating flux (YYYY-MM-DD)
	## to = date on which to stop interpolating flux (YYYY-MM-DD)
	## obs = character vector giving name of column containing observations
	## obs.se = character vector giving name of column containing observation SEs
	## mod = character vector giving name of column containing modelled values
	## method = interpolation method (see ?approx). Defaults to 'constant'.

	## Set measurement period
	from = as.Date(from, format = "%Y/%m/%d")
	to = as.Date(to, format = "%Y/%m/%d")
	
	## Find Date column in data
	datclass = sapply(data, class)
	## Check that data has Date column, stop if missing
	if (!("Date" %in% datclass)) stop("Column of class 'Date' missing from 'obs'")
	## Otherwise rename Date column in data to "DATE"
	datecol = which(sapply(data, class) == "Date")
	colnames(data)[datecol] = "DATE"
	
	# #Subset by from and to dates
	subdat = data[data$DATE >= from & data$DATE <= to, ]
	
	## Interpolate observations and upper and lower errors
	if (!(is.null(obs)) & method == "constant") {
	obs.i = unlist(approx(x = subdat[["DATE"]],
							   y = subdat[[obs]],
							   xout = subdat[["DATE"]],
							   method = "constant")[2])
							   
	obs.i.upper = unlist(approx(x = subdat[["DATE"]],
							   y = subdat[[obs]] + subdat[[obs.se]],
							   xout = subdat[["DATE"]],
							   method = "constant")[2])
							   
	obs.i.lower = unlist(approx(x = subdat[["DATE"]],
							   y = subdat[[obs]] - subdat[[obs.se]],
							   xout = subdat[["DATE"]],
							   method = "constant")[2])							   
	
	## Calculate cumulative flux from interpolated observations
	obs.cum = cumsum(obs.i)
	obs.cum.up = cumsum(obs.i.upper)
	obs.cum.lo = cumsum(obs.i.lower)
	}
	
	## Calculate cumulative flux from modelled data
	## Already on daily timestep so no interpolation necessary
	if (!(is.null(mod)) & method == "constant") {
	mod.cum = cumsum(subdat[[mod]])
	}
	
	## Create dataframe with subsetted data and generated cumulative series
	out = cbind(subdat, obs.i, obs.i.upper, obs.i.lower, obs.cum, obs.cum.up, obs.cum.lo, mod.cum)
	
	## Return dataframe
	return(out)
}