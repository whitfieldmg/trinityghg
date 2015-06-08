### Description
# MODEVAL-R (18-05-2015)
# Calculate model fit statistics
# This replicates Pete and Jo Smith's MODEVAL2000 spreadsheet
# Available from http://global.oup.com/uk/orc/geography/smith_smith/01student/weblinks/ch03/MODEVAL%202_0.xls
# Ported to R by Mike Whitfield (http://mikewhitfield.wordpress.com)

modeval = function (dat, names.obs, names.mod, names.err = NULL, n = NULL, scrub = FALSE) {

### Arguments:
# dat = data frame containing measured and simulated data
# names.obs = character; measured data column header
# names.mod = character; simulated data column header
# names.err = character; column header for measured data standard error, if available
# n = integer; number of reps
# scrub = logical; remove negative fluxes from measured data?

	### Helper functions
	## t value from inverse distribution
	t_inv = function (x, reps) abs(qt(x / 2, reps - 2))

	##f value from inverse distribution
	f_inv = function (x, obs, reps) qf(1 - x, obs - 2, (obs * reps) - 2)

	## se - returns NA if available obs < 2
	se = function (x) sqrt(var(x, na.rm = TRUE) / (length(x) - sum(is.na(x))))

	## Load data and perform subsetting
	dat = dat

	obs = dat[, names.obs]

	mod = dat[, names.mod]

	## If scrub == TRUE, remove negative flux values from data
	if (scrub) obs[obs < 0] = NA

	## Get n, N, mean, se
	if (length(names.obs) < 2) {
		N = length(obs)
		if (is.null(n)) {
			n = rep(1, times = N)
			} else {
			n = n
			}
		obs.mean = obs
		if (is.null(names.err)) {
			obs.se = NA
			} else {
			obs.se = dat[, names.err]
			}
		} else {
		N = nrow(obs)
		n = rowSums(!is.na(obs))
		obs.mean = rowMeans(obs, na.rm = TRUE)
		if (is.null(names.err)) {
			obs.se = apply(obs, 1, se)
			} else {
			obs.se = dat[, names.err]
			}
		}


	## LOFIT
	lofit = function () {
		
		LOFIT = sum(((obs.mean - mod)^2) * n, na.rm = TRUE)

		findlofitf = function (colname) {
			obs.sub = obs[, colname]
			obs.sub[is.na(obs.sub)] = 0
			sub.out = ((obs.sub - mod) - (obs.mean - mod))^2
			return(sub.out)
		}
		
		if (length(names.obs) < 2) {
		# LOFIT F stat not possible without replicates
			warning("Individual replicates not provided, LOFIT F not available.")
			LF.out = NA
			} else {
			LF.out = sum(rowSums(sapply(names(obs), findlofitf), na.rm = TRUE), na.rm = TRUE)
		}
		
		LOFIT.F = sum(n - 1) * LOFIT / (N * LF.out)

		F.crit = f_inv(0.05, N, 3)
		
		return(c("LOFIT" = LOFIT,
				 "F (MSLOFIT / MSE)" = LOFIT.F,
				 "F (critical at 5%)" = F.crit))
	}

	## RMSE
	rmse = function () {
		s = as.data.frame(cbind(obs.mean, obs.se, mod))
		s = s[!is.na(obs.mean), ]
		N.nona = length(obs.mean) - sum(is.na(obs.mean))
		n = n
		
		RMSE = (100 / mean(s$obs.mean)) * sqrt(sum((s$obs.mean - s$mod)^2) / N.nona)
		
		# if (length(names.obs) < 2) {
			# RMSE95 = NA
			# } else {
		# RMSE95 = (100 / mean(s$obs.mean)) * sqrt(sum((s$obs.se * t_inv(0.05, n))^2, na.rm = TRUE) / N.nona)
			# }
			
		RMSE95 = (100 / mean(s$obs.mean)) * sqrt(sum((s$obs.se * t_inv(0.05, n))^2, na.rm = TRUE) / N.nona)
		
			
		return(c("RMSE" = RMSE,
				 "RMSE (95% conf. limit)" = RMSE95))
	}
	
	## E
	errfun = function () {
		s = as.data.frame(cbind(obs.mean, obs.se, mod))
		s = s[!is.na(obs.mean), ]
		N.nona = length(obs.mean) - sum(is.na(obs.mean))
		n = n
	
		E = (100 / mean(s$obs.mean)) * (sum(s$obs.mean - s$mod) / N.nona)
		
		# if (length(names.obs) < 2) {
			# E95 = NA
			# } else {
			# E95 = (100 / mean(obs.mean)) * (sum(obs.se * t_inv(0.05, n), na.rm = TRUE) / N)
			# }
			
		E95 = (100 / mean(s$obs.mean)) * (sum(s$obs.se * t_inv(0.05, n), na.rm = TRUE) / N.nona)

		
		return(c("Relative error (E)" = E,
				 "E (95% conf. limit)" = E95))
	}
	
	## M
	mfun = function () {
		s = as.data.frame(cbind(obs.mean, obs.se, mod))
		s = s[!is.na(obs.mean), ]
		N.nona = length(obs.mean) - sum(is.na(obs.mean))		
		
		M = sum(s$obs.mean - s$mod) / N.nona

		# M.t = (M * sqrt(N.nona)) / sqrt(abs(sum(((s$obs.mean - s$mod) - M))^2 / (N.nona - 1)))
		sumdiff = sum(((s$obs.mean - s$mod) - M) * ((s$obs.mean - s$mod) - M))
		M.t = (M * sqrt(N.nona)) / sqrt(abs(((sumdiff - M) / (N.nona - 1))))
		
		t.crit = t_inv(0.05, N.nona)
		
		return(c("Mean difference (M)" = M,
				 "Student's t of M" = M.t,
				 "t value (two-tailed)" = t.crit))

	}
	
	## Correlation coefficient
	rfun = function () {
		N.nona = length(obs.mean) - sum(is.na(obs.mean))
		
		r = cor(obs.mean, mod, use = "pairwise.complete.obs")

		r.F = ((N.nona - 2) * r^2) / (1 - r^2)

		r.Fcrit = t_inv(0.05, N.nona)^2
		
		return(c("Correlation coefficient (r)" = r,
				 "F value" = r.F,
				 "F value at (p = 0.05)" = r.Fcrit))

	}	
	
	## Call functions and store results in list 'outlist'
	outlist = list(LOFIT = lofit(),
				   RMSE = rmse(),
				   Error = errfun(),
				   M = mfun(),
				   r = rfun(),
				   Obs = length(obs.mean) - sum(is.na(obs.mean)),
				   n = n)

	## Convert to data frame
	outframe = as.data.frame(unlist(outlist), optional = TRUE)
	
	## Set colnames and descriptive rownames
	colnames(outframe) = "Value"
	rownames(outframe) = c("LOFIT", "F (MSLOFIT / MSE)", "F (critical at 5%)",
						   "RMSE", "RMSE (95% conf. limit)",
						   "Relative error (E)", "E (95% conf. limit)",
						   "Mean difference (M)", "Student's t of M", "t-value (two-tailed)",
						   "Correlation coefficient (r)", "F-value", "F-value at p = 0.05",
						   "Obs", "Reps")
	
	## Format Value column in outframe
	outframe$Value = format(outframe$Value, digits = 3, scientific = FALSE)

	## Function returns outframe
	return(outframe)

}

