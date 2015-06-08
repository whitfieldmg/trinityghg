# Function for plotting model summary plots

fitplot = function (data, obs, mod, obs.se = NULL, from = NULL, to = NULL, xymax = NULL) {
	
	# Find Date column in data
	datclass = sapply(data, class)
	# Check that data has Date column, stop if missing
	if (!("Date" %in% datclass)) stop("Column of class 'Date' missing from 'obs'")
	# Otherwise rename Date column in data to "DATE"
	datecol = which(sapply(data, class) == "Date")
	colnames(data)[datecol] = "date"
	
	dat = data.frame(date = data[, "date"],
					obs = data[, obs],
					mod = data[, mod],
					obs.se = data[, obs.se])
	
	if ((!(is.null(from))) & (!(is.null(to)))) {
		dat = dat[dat$date >= from & dat$date <= to, ]
	}
	
	dat$negflag = ifelse(dat$obs < 0, "grey", "black")
	
	xy = ggplot(dat, aes(obs, mod)) +
		 geom_abline(lty = 2) +
		 geom_point(pch = 21) +
		 coord_fixed() +
		 labs(x = "Measured values", y = "Modelled values") +
		 xlim(0, max(c(dat$obs, dat$mod), na.rm = TRUE)) +
		 ylim(0, max(c(dat$obs, dat$mod), na.rm = TRUE)) +
		 theme(panel.background = element_rect(fill = "white"),
			   panel.border = element_rect(fill = NA, colour = "black"),
			   panel.grid = element_blank(),
			   axis.ticks = element_line(colour = "black"),
			   axis.text = element_text(colour = "black"))
			   
	if (!(is.null(xymax))) {
	xy = xy + xlim(0, xymax) + ylim(0, xymax)
	}
	
	tsplot = ggplot(dat, aes(x = date)) +
			 geom_point(aes(y = obs), pch = 21, fill = "white", colour = dat$negflag) +
			 geom_line(aes(y = mod), colour = "blue") +
			 labs(x = "Date", y = "Flux") +
			 theme(panel.background = element_rect(fill = "white"),
			 panel.border = element_rect(fill = NA, colour = "black"),
			 panel.grid = element_blank(),
			 axis.ticks = element_line(colour = "black"),
			 axis.text = element_text(colour = "black"))
			 
	if (!(is.null(obs.se))) {
	tsplot = tsplot + geom_errorbar(aes(ymin = obs - obs.se, ymax = obs + obs.se), colour = dat$negflag)
	xy = xy + geom_errorbarh(aes(xmin = obs - obs.se, xmax = obs + obs.se), height = 0)
	}
	
	combined = arrangeGrob(xy, tsplot, ncol = 1)
	
	return(combined)
	
	}
	