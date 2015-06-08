# DNDC results loader function
# dir = results directory
# nfile = range of results files to load
# filelist = attempts to load all files by default (may not work with 94)
# ver = DNDC version: either 95 or 94
read.dndc = function(dir,
					 nfile,
					 filelist = c("Climate", "FieldCrop", "FieldManage", "Graze", "SoilC", "SoilClimate", "SoilMicrobe", "SoilN", "SoilP", "SoilWater"),
					 ver) {
	dir.old = getwd()
	setwd(dir)
	# Climate
	if ("Climate" %in% filelist) {
		flist = list.files(pattern = "_Climate_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		Climate = lapply(flist,
						 read.csv,
						 skip = 2,
						 header = F,
						 col.names = c("DAY", "TEMP", "PREC", "WSPD", "RAD", "HUM", "PET", "AET", "ET", "TRANS"))
		names(Climate) = paste("YR", nfile, sep = "")
	}
	# Crops
	if ("FieldCrop" %in% filelist) {
		flist = list.files(pattern = "_FieldCrop_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		# Variable width file, so no column names specified or header included
		FieldCrop = lapply(flist,
							read.csv,
							skip = 4,
							header = F)
		names(FieldCrop) = paste("YR", nfile, sep = "")
	}
	# Daily management
	if ("FieldManage" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_FieldManage_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		FieldManage = lapply(flist,
							read.csv,
							skip = 4,
							header = F,
							col.names = c("DAY", "IRRI", "FERTN", "FERTP", "MAN.N", "MAN.P", "CUT"))
		names(FieldManage) = paste("YR", nfile, sep = "")
	}
	if ("FieldManage" %in% filelist & ver == 94) {
		flist = list.files(pattern = "_FieldManage_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		FieldManage = lapply(flist,
							read.csv,
							skip = 4,
							header = F,
							col.names = c("DAY", "IRRI", "FERTN", "MAN.N", "CUT"))
		names(FieldManage) = paste("YR", nfile, sep = "")
	}
	# Grazing
	if ("Graze" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_Graze_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		Graze = lapply(flist,
							read.csv,
							skip = 5,
							header = F,
							col.names = c("DAY", "GRSC", "DAIRY", "BEEF", "PIG", "SHEEP", "HORSE", "HOURS", "GRZC", "GRZN", "DNGC", "DNGN", "URIC", "URIN", "DNGP", "DEF"))
		names(Graze) = paste("YR", nfile, sep = "")
	}
	if ("Graze" %in% filelist & ver == 94) {
		flist = list.files(pattern = "_Graze_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		Graze = lapply(flist,
							read.csv,
							skip = 5,
							header = F,
							col.names = c("DAY", "GRSC", "DAIRY", "BEEF", "PIG", "SHEEP", "HORSE", "HOURS", "GRZC", "GRZN", "DNGC", "DNGN", "URIN", "DEF", "NH3WASTE"))
		names(Graze) = paste("YR", nfile, sep = "")
	}
	# Soil C
	if ("SoilC" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_SoilC_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilC = lapply(flist,
							read.csv,
							skip = 2,
							header = F,
							col.names = c("DAY", "VLAB", "LLAB", "LRES", "CMIC", "CHMD", "CHMS", "CHAR", "SOC", "SOC1", "SOC2", "SOC3", "SOC4", "SOC5", "SOC1A", "SOC2A", "SOC3A", "SOC4A", "SOC5A", "DSOC", "DOC", "PHOT", "RLEAF", "RSTEM", "RROOT", "SOILHET", "NPP", "RECO", "NEE", "STUB", "CH4PROD", "CH4OX", "CH4FLUX", "CH4POOL", "DOCLCH", "LITC", "MANC"))
		names(SoilC) = paste("YR", nfile, sep = "")
	}
	if ("SoilC" %in% filelist & ver == 94) {
		flist = list.files(pattern = "_SoilC_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilC = lapply(flist,
							read.csv,
							skip = 2,
							header = F,
							col.names = c("DAY", "VLAB", "LLAB", "LRES", "CMIC", "CHMD", "CHMS", "CHAR", "SOC", "DSOC", "DOC", "PHOT", "RSTEM", "RROOT", "NPP", "SOILHET", "RECO", "NEE", "STUB", "CH4DOC", "CH4PROD", "CH4OX", "CH4FLUX", "CH4POOL", "DOCLCH", "LITC", "MANC"))
		names(SoilC) = paste("YR", nfile, sep = "")
	}
	# Soil climate
	if ("SoilClimate" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_SoilClimate_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilClimate = lapply(flist,
							read.csv,
							skip = 4,
							header = F,
							col.names = c("DAY", "TEMP", "PREC", "PET", "STEMP1", "STEMP5", "STEMP10", "STEMP20", "STEMP30", "STEMP40", "STEMP50", "WFPS1", "WFPS5", "WFPS10", "WFPS20", "WFPS30", "WFPS40", "WFPS50", "SOX1", "SOX10", "SOX20", "SOX30", "SOX40", "SOX50", "SEH1", "SEH10", "SEH20", "SEH30", "SEH40", "SEH50", "WTD", "ICE1", "ICE10", "ICE20", "ICE30", "ICE40", "ICE50", "ICE.MM", "SNOW", "SOILWAT", "DEEPWAT", "PH1", "PH10", "PH20", "PH30", "PH40", "PH50", "TSURFWAT"))
		names(SoilClimate) = paste("YR", nfile, sep = "")
	}
	if ("SoilClimate" %in% filelist & ver == 94) {
		flist = list.files(pattern = "_SoilClimate_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilClimate = lapply(flist,
							read.csv,
							skip = 4,
							header = F,
							col.names = c("DAY", "TEMP", "PREC", "PET", "STEMP1", "STEMP5", "STEMP10", "STEMP20", "STEMP30", "STEMP40", "STEMP50", "WFPS1", "WFPS5", "WFPS10", "WFPS20", "WFPS30", "WFPS40", "WFPS50", "SOX1", "SOX10", "SOX20", "SOX30", "SOX40", "SOX50", "SEH1", "SEH10", "SEH20", "SEH30", "SEH40", "SEH50", "WTD", "ICE1", "ICE10", "ICE20", "ICE30", "ICE40", "ICE50", "ICE.MM", "SNOW", "SOILWAT", "WATPOOL", "PH1", "PH10", "PH20", "PH30", "PH40", "PH50"))
		names(SoilClimate) = paste("YR", nfile, sep = "")
	}
	# Soil microbes (95 only)
	if ("SoilMicrobe" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_SoilMicrobe_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilMicrobe = lapply(flist,
							read.csv,
							skip = 4,
							header = F,
							col.names = c("DAY", "DECOMP", "NIT", "DENIT", "METHGEN", "MTROPH", "MGENIND1", "MGENIND10", "MGENIND20", "MGENIND30", "MGENIND40", "MGENIND50", "H2.10", "H2.20", "H2.30", "H2.40", "H2.50", "DOC10", "DOC20", "DOC30", "DOC40", "DOC50", "CO2.10", "CO2.20", "CO2.30", "CO2.40", "CO2.50", "DOCCH4.10", "DOCCH4.20", "DOCCH4.30", "DOCCH4.40", "DOCCH4.50", "CO2CH4.10", "CO2CH4.20", "CO2CH4.30", "CO2CH4.40", "CO2CH4.50", "CH4OX.10", "CH4OX.20", "CH4OX.30", "CH4OX.40", "CH4OX.50"))
		names(SoilMicrobe) = paste("YR", nfile, sep = "")
	}
	# Soil N
	if ("SoilN" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_SoilN_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilN = lapply(flist,
							read.csv,
							skip = 5,
							header = F,
							col.names = c("DAY", "CROPUP", "UREA", "AMM10", "NIT10", "EXAMM10", "AMM20", "NIT20", "EXAMM20", "AMM30", "NIT30", "EXAMM30", "AMM40", "NIT40", "EXAMM40", "AMM50", "NIT50", "EXAMM50", "NH3.50", "N2O", "NO", "N2", "NH3", "LCHNO3", "LCHUREA", "LCHDON", "IONRUN", "NMIN", "NASS", "DOC.ICE", "N.ICE", "N2O.ICE", "N2.ICE", "NIT", "DENIT", "NFIX", "LITN", "SON.10", "SON.20", "SON.30", "SON.40", "SON.50", "SON.10A", "SON.20A", "SON.30A", "SON.40A", "SON.50A"))
		names(SoilN) = paste("YR", nfile, sep = "")
	}
	if ("SoilN" %in% filelist & ver == 94) {
		flist = list.files(pattern = "_SoilN_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilN = lapply(flist,
							read.csv,
							skip = 5,
							header = F,
							col.names = c("DAY", "CROPUP", "UREA", "AMM10", "NIT10", "EXAMM10", "AMM20", "NIT20", "EXAMM20", "AMM30", "NIT30", "EXAMM30", "AMM40", "NIT40", "EXAMM40", "AMM50", "NIT50", "EXAMM50", "NH3.50", "N2O", "NO", "N2", "NH3", "LCHNO3", "LCHUREA", "NMIN", "NASS", "DOC.ICE", "N.ICE", "N2O.ICE", "N2.ICE", "NIT", "DENIT", "NFIX", "LITN"))
		names(SoilN) = paste("YR", nfile, sep = "")
	}
	# Soil P (95 only)
	if ("SoilP" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_SoilP_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilP = lapply(flist,
							read.csv,
							skip = 2,
							header = F,
							col.names = c("DAY", "ORGP", "LABP", "SRBP", "COMP", "CRDP", "PDAY", "CRPP", "LCHP", "SO4", "H2S"))
		names(SoilP) = paste("YR", nfile, sep = "")
	}
	# Soil water
	if ("SoilWater" %in% filelist & ver == 95) {
		flist = list.files(pattern = "_SoilWater_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilWater = lapply(flist,
							read.csv,
							skip = 2,
							header = F,
							col.names = c("DAY", "INISW", "INIDW", "INIWP", "PREC", "IRRI", "INFL", "ENDSW", "ENDDW", "ENDWP", "EVAP", "TRANS", "LCH", "RUNF", "OUTFL", "BAL", "SDYD", "SOCOUT", "IONOUT", "OGPOUT", "ADPOUT", "LPOUT", "FREEWAT", "SOILICE", "POND", "SNWPK"))
		names(SoilWater) = paste("YR", nfile, sep = "")
	}
	if ("SoilWater" %in% filelist & ver == 94) {
		flist = list.files(pattern = "_SoilWater_")
		flist = flist[grep(paste(as.character(nfile), collapse = "|"), flist)]
		SoilWater = lapply(flist,
							read.csv,
							skip = 4,
							header = F,
							col.names = c("DAY", "INISW", "ENDSW", "FREEWAT", "SOILICE", "INIDW", "ENDDW", "PREC", "IRRI", "POND", "SNOPK", "EVAP", "TRANS", "SURFWAT", "LEACH", "RUN", "DSOILWAT", "DAYIN", "DAYOUT", "ERROR"))
		names(SoilWater) = paste("YR", nfile, sep = "")
	}

	OUTLIST = mget(ls())
	setwd(dir.old)
	OUTLIST["filelist"] = NULL
	OUTLIST["flist"] = NULL
	OUTLIST["dir"] = NULL
	OUTLIST["dir.old"] = NULL

	return(OUTLIST)
}
