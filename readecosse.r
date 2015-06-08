# Function for reading ECOSSE output files
# For now, this only reads SUMMARY.OUT

read.ecosse = function (directory, filelist = "SUMMARY.OUT") {

	if ("SUMMARY.OUT" %in% filelist) {
		# Need readLines because of strange formatting in SUMMARY.OUT
		tmp = readLines(paste(directory, "SUMMARY.OUT", sep = "/"))
		# Replace fields filled with '**********' with ' NA'
		tmp = gsub("\\*{10}", " NA", tmp)
		# Replace concatenated fields with two NAs
		tmp = gsub("([0-9]{1})[.]([0-9]{9,})[.]([0-9]{5,})", "NA NA ", tmp)
		# read.table interprets NA in the expected way
		Summary = read.table(text = tmp, skip = 1, header = TRUE)
	}
	
	outlist = mget(ls())
	outlist["directory"] = NULL
	outlist["filelist"] = NULL
	outlist["tmp"] = NULL
	
	return(outlist)
	
	}
	
# Summary-specific version for running with Montecarlo simulations

read.ecosse.summary = function (directory) {

		# Need readLines because of strange formatting in SUMMARY.OUT
		tmp = readLines(paste(directory, "SUMMARY.OUT", sep = "/"))
		# Replace fields filled with '**********' with ' NA'
		tmp = gsub("\\*{10}", " NA", tmp)
		# Replace concatenated fields with two NAs
		tmp = gsub("([0-9]{1})[.]([0-9]{9,})[.]([0-9]{5,})", "NA NA ", tmp)
		# read.table interprets NA in the expected way
		Summary = read.table(text = tmp, skip = 1, header = TRUE)
	
	return(Summary)
	
	}