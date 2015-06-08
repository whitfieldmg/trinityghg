# extract.dndc
# function to extract and bind DNDC outputs for a specified variable
# to simulation time series

extract.dndc = function (dndc.object, time.series, group, variable) {

	# Clean model version info from dndc.object
	dndc.object["ver"] = NULL
	
	sim = do.call("rbind", dndc.object[[group]])
	# simvar = sim[, names(sim) %in% variable]
	simvar = sim[[variable]]
	
	# Check that number of rows (i.e. timesteps) in dndc.object and time.series match
	if (nrow(sim) != nrow(time.series)) {
		stop("Number of timesteps in dndc.object and time.series do not match.")
	}
	
	simts = cbind(time.series, simvar)
	colnames(simts)[length(simts)] = paste(variable, "sim", sep = ".")
	
	return(simts)
}