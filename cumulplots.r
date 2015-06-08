# Functions to:
	# plot a graph of interpolated and modelled values
	# plot a graph of cumulatives from each model against observed cumulative
	
plot.interp = function(DATE, OBS, I.OBS, OBS.UP, OBS.LO, F.94, F.95, F.DC, F.EC,
						FUNIT = NULL, TITLE = NULL) {

	dat = data.frame(DATE = DATE,
					 I.OBS = I.OBS,
					 F.94 = F.94,
					 F.95 = F.95,
					 F.DC = F.DC,
					 F.EC = F.EC)
					 
	bounds = data.frame(DATE = DATE,
						OBS = OBS,
						OBS.UP = OBS.UP,
						OBS.LO = OBS.LO)
					 
	meltdat = melt(dat,
				   id = "DATE")
				   
	meltdat = mutate(meltdat,
					variable = factor(variable,
							levels = levels(variable),
							labels = c("Obs (interp)", "DNDC94", "DNDC95", "DayCent", "ECOSSE")))
							
	intplot = ggplot(meltdat) +
				geom_hline(y = 0, lty = 2) +
				geom_line(aes(DATE, value, colour = variable)) +
				geom_ribbon(data = bounds,
							aes(x = DATE, ymin = OBS.LO, ymax = OBS.UP),
							alpha = 0.2) +
				geom_point(data = bounds,
							aes(x = DATE, y = OBS), pch = 21) +
				labs(x = "Date",
					y = FUNIT,
					colour = "Model") +
				ggtitle(TITLE) +
				theme_modplot
				
	return(intplot)
	}

plot.cumul = function(DATE, OBS.C, C.94, C.95, C.DC, C.EC,
						FUNIT = NULL, TITLE = NULL) {
						
		dat = data.frame(DATE = DATE,
					 OBS.C = OBS.C,
					 C.94 = C.94,
					 C.95 = C.95,
					 C.DC = C.DC,
					 C.EC = C.EC)
					 
		meltdat = melt(dat,
						id = "DATE")
						
		meltdat = mutate(meltdat,
				variable = factor(variable,
						levels = levels(variable),
						labels = c("Obs", "DNDC94", "DNDC95", "DayCent", "ECOSSE")))
						
cumulplot = ggplot(meltdat) +
			geom_hline(y = 0, lty = 2) +
			geom_line(aes(DATE, value, colour = variable)) +
			labs(x = "Date",
				 y = FUNIT,
				 title = TITLE) +
			theme_modplot
		
	return(cumulplot)
	
	}
