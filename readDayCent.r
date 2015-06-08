# Function written in the R language by Mike Whitfield
# mgwhitfield@gmail.com

# This function reads output from a DailyDayCent simulation
# INPUTS:
# The function takes two arguments:
# choosefiles: A character vector containing the names of the
#              files you wish to look at. If left blank, the
#              function will load all the files in the working
#              directory with extensions .out and .csv
# lis:         Logical: do you want to read the .lis file?
#              Defaults to FALSE.
# resdir:      A string specifying the directory containing
#              the results files you want to read. If left 
#              blank, this defaults to the working directory.
# 
# OUTPUTS:
# The function outputs a list of dataframes, each dataframe
# corresponding to its counterpart .out or .csv file. The 
# function does not manipulate the contents of output files,
# apart from creating suitable headers in applicable cases.
# 
# If an output file is empty (e.g. harvest.out in the case off
# no harvests being scheduled), its corresponding dataframe
# will also be empty.
# 
# Access dataframes and variables within dataframes using $
# e.g. outputlist$dataframe$variable


read.daycent = function(choosefiles = NULL, lis = FALSE, resdir = NULL) {

	# if (!is.null(choosefiles)) {
		# filelist = choosefiles
		# } else {
		# outfiles = read.table("outfiles.in",
							# skip = 1,
							# comment.char = "#",
							# header = F,
							# col.names = c("output", "file"))
		# filelist = as.character(outfiles[outfiles$output == 1, 2])
		# }
	if (!is.null(resdir)) {
	resdir = resdir
	} else {
	resdir = getwd()
	}
	
	if (!is.null(choosefiles)) {
	filelist = choosefiles
	} else {
	filelist = c(list.files(pattern = ".out"), list.files(pattern = ".csv"))
	}

	if("bio.out" %in% filelist) {
		bio = read.table(paste(resdir, "bio.out", sep = "/"),
						 header = T)
		}

	if("cflows.out" %in% filelist) {
		cflows = read.table(paste(resdir, "cflows.out", sep = "/"),
							header = T)
		}
							
	if("co2.out" %in% filelist) {
		co2 = read.table(paste(resdir, "co2.out", sep = "/"),
						 skip = 1,
						 header = F)
		colnames(co2) = c("time", "dayofyr", paste(rep("CO2_ppm_lr", times = (ncol(co2) -2)), 1:(ncol(co2) -2), sep = ""))
		}
		
	if("daily.out" %in% filelist) {
		daily = read.table(paste(resdir, "daily.out", sep = "/"),
						   header = T)
		}
		
	if("dc_sip.csv" %in% filelist) {
		dc_sip = read.csv(paste(resdir, "dc_sip.csv", sep = "/"),
						  header = T)
		}

	if("deadc.out" %in% filelist) {
		deadc = read.table(paste(resdir, "deadc.out", sep = "/"),
						   header = T)
		}

	if("dels.out" %in% filelist) {
		dels = read.table(paste(resdir, "dels.out", sep = "/"),
						  header = T)
		}
		
	if("dN2lyr.out" %in% filelist) {
		dN2lyr = read.table(paste(resdir, "dN2lyr.out", sep = "/"),
							skip = 1,
							header = F)
		colnames(dN2lyr) = c("time", "dayofyr", paste(rep("dN2_gm2_lr", times = (ncol(dN2lyr) -2)), 1:(ncol(dN2lyr) -2), sep = ""))
		}

	if("dN2Olyr.out" %in% filelist) {
		dN2Olyr = read.table(paste(resdir, "dN2Olyr.out", sep = "/"),
							 skip = 1,
							 header = F)
		colnames(dN2Olyr) = c("time", "dayofyr", paste(rep("dN2O_gm2_lr", times = (ncol(dN2Olyr) -2)), 1:(ncol(dN2Olyr) -2), sep = ""))
		}

	if("harvest.csv" %in% filelist) {
		harvest = read.csv(paste(resdir, "harvest.csv", sep = "/"),
						   header = T)
		}
		
	if("livec.out" %in% filelist) {
		livec = read.table(paste(resdir, "livec.out", sep = "/"),
						   header = T)
		}
		
	if("nflux.out" %in% filelist) {
		nflux = read.table(paste(resdir, "nflux.out", sep = "/"),
						   header = T)
		}
		
	if("resp.out" %in% filelist) {
		resp = read.table(paste(resdir, "resp.out", sep = "/"),
						  header = T)
		}
		
	if("soilc.out" %in% filelist) {
		soilc = read.table(paste(resdir, "soilc.out", sep = "/"),
						   header = T)
		}

	if("soiln.out" %in% filelist) {
		soiln = read.table(paste(resdir, "soiln.out", sep = "/"),
						   skip = 1,
						   header = F)
		colnames(soiln) = c("time", "dayofyr", "NH4", paste(rep("NO3_ppm_lr", times = (ncol(soiln) -3)), 1:(ncol(soiln) -3), sep = ""))
		}

	if("soiltavg.out" %in% filelist) {
		soiltavg = read.table(paste(resdir, "soiltavg.out", sep = "/"),
							  skip = 0,
							  header = F)
		colnames(soiltavg) = c("time", "dayofyr", paste(rep("soilt_lr", times = (ncol(soiltavg) -2)), 1:(ncol(soiltavg) -2), sep = ""))
		}
		
	if("soiltmax.out" %in% filelist) {
		soiltmax = read.table(paste(resdir, "soiltmax.out", sep = "/"),
							  skip = 0,
							  header = F)
		colnames(soiltmax) = c("time", "dayofyr", paste(rep("soilt_lr", times = (ncol(soiltmax) -2)), 1:(ncol(soiltmax) -2), sep = ""))
		}
		
	if("soiltmin.out" %in% filelist) {
		soiltmin = read.table(paste(resdir, "soiltmin.out", sep = "/"),
							  skip = 0,
							  header = F)
		colnames(soiltmin) = c("time", "dayofyr", paste(rep("soilt_lr", times = (ncol(soiltmin) -2)), 1:(ncol(soiltmin) -2), sep = ""))
		}

	# Note: layers in stemp_dx aren't meaningful	
	if("stemp_dx.out" %in% filelist) {
		stemp_dx = read.table(paste(resdir, "stemp_dx.out", sep = "/"),
							  skip = 0,
							  header = F)
		colnames(stemp_dx) = c("time", "dayofyr", paste(rep("soilt_lr", times = (ncol(stemp_dx) -2)), 1:(ncol(stemp_dx) -2), sep = ""))
		}
		
	if("summary.out" %in% filelist) {
		summary = read.table(paste(resdir, "summary.out", sep = "/"),
							 header = T)
		}

	if("sysc.out" %in% filelist) {
		sysc = read.table(paste(resdir, "sysc.out", sep = "/"),
						  header = T)
		}
		
	if("tgmonth.out" %in% filelist) {
		tgmonth = read.table(paste(resdir, "tgmonth.out", sep = "/"),
							 header = T)
		}

	if("vswc.out" %in% filelist) {
		vswc = read.table(paste(resdir, "vswc.out", sep = "/"),
						  header = F,
						  skip = 0)
		colnames(vswc) = c("time", "dayofyr", paste(rep("vwc_lr", times = (ncol(vswc) -2)), 1:(ncol(vswc) -2), sep = ""))
		}
		
	if("watrbal.out" %in% filelist) {
		watrbal = read.table(paste(resdir, "watrbal.out", sep = "/"),
							 skip = 2,
							 header = T)
		}
		
	if("wflux.out" %in% filelist) {
		wflux = read.table(paste(resdir, "wflux.out", sep = "/"),
						   skip = 1,
						   header = F)
		colnames(wflux) = c("time", "dayofyr", paste(rep("H2O_cm_lr", times = (ncol(wflux) -2)), 1:(ncol(wflux) -2), sep = ""))				   
		}
		
	if("wfps.out" %in% filelist) {
		wfps = read.table(paste(resdir, "wfps.out", sep = "/"),
						  skip = 0,
						  header = F)
		colnames(wfps) = c("time", "dayofyr", paste(rep("wfps_lr", times = (ncol(wfps) -2)), 1:(ncol(wfps) -2), sep = ""))
		}
			
	if("year_cflows.out" %in% filelist) {
		year_cflows = read.table(paste(resdir, "year_cflows.out", sep = "/"),
								 header = T)
		}

	if("year_summary.out" %in% filelist) {
		year_summary = read.table(paste(resdir, "year_summary.out", sep = "/"),
								  header = T)
		}
	
	# If specified, include .lis composite ('comp') output
	if(lis) {
		comp = read.table(list.files(path = resdir, pattern = ".lis"),
						  header = T,
						  skip = 0)
		}
	
	# Get generated tables and combine in list 'outlist'
	outlist = mget(ls())
	
	# Remove unwanted output from 'outlist'
	outlist["filelist"] = NULL
	outlist["outfiles"] = NULL
	outlist["choosefiles"] = NULL
	outlist["lis"] = NULL
	outlist["resdir"] = NULL
	
	# Output 'outlist'
	return(outlist)

}
