# Write a function to extract RMSE from EVAL
get.rmse = function (x) {
			RMSE = sapply(names(x[["EVAL"]]), function (m) {
												as.numeric(x[["EVAL"]][[m]]["RMSE", ])
												}
						)
			if (mode(RMSE) == "list") RMSE <- unlist(RMSE)
			return(RMSE)
					}
					
get.multi.rmse = function (x) {
			xp = as.character(ls.str(x))
			RMSE = list()
			
			for (i in xp)
			RMSE[[i]] = sapply(names(x[[i]][["EVAL"]]), function (m) {
												as.numeric(x[[i]][["EVAL"]][[m]]["RMSE", ])
												}
						)
			RMSE = as.data.frame(RMSE)
			return(RMSE)
			}
			
# Write a function to extract RMSE95 from EVAL
get.rmse95 = function (x) {
			RMSE95 = sapply(names(x[["EVAL"]]), function (m) {
												as.numeric(x[["EVAL"]][[m]]["RMSE (95% conf. limit", ])
												}
						)
			if (mode(RMSE95) == "list") RMSE95 <- unlist(RMSE95)
			return(RMSE95)
					}
					
get.multi.rmse95 = function (x) {
			xp = as.character(ls.str(x))
			RMSE95 = list()
			
			for (i in xp)
			RMSE95[[i]] = sapply(names(x[[i]][["EVAL"]]), function (m) {
												as.numeric(x[[i]][["EVAL"]][[m]]["RMSE (95% conf. limit", ])
												}
						)
			RMSE95 = as.data.frame(RMSE95)
			return(RMSE95)
			}
			
			
get.cor = function (x) {
			COR = sapply(names(x[["EVAL"]]), function (m) {
												as.numeric(x[["EVAL"]][[m]]["Correlation coefficient (r)", ])
												}
						)
			if (mode(COR) == "list") COR <- unlist(COR)
			return(COR)
					}
					
get.multi.cor = function (x) {
			xp = as.character(ls.str(x))
			COR = list()
			
			for (i in xp)
			COR[[i]] = sapply(names(x[[i]][["EVAL"]]), function (m) {
												as.numeric(x[[i]][["EVAL"]][[m]]["Correlation coefficient (r)", ])
												}
						)
			COR = as.data.frame(COR)
			return(COR)
			}
			
			
get.e = function (x) {
			E = sapply(names(x[["EVAL"]]), function (m) {
												as.numeric(x[["EVAL"]][[m]]["Relative error (E)", ])
												}
						)
			if (mode(E) == "list") E <- unlist(E)
			return(E)
					}
					
get.multi.e = function (x) {
			xp = as.character(ls.str(x))
			E = list()
			
			for (i in xp)
			E[[i]] = sapply(names(x[[i]][["EVAL"]]), function (m) {
												as.numeric(x[[i]][["EVAL"]][[m]]["Relative error (E)", ])
												}
						)
			E = as.data.frame(E)
			return(E)
			}
			
			
# Compare cumulative fits
get.cc = function (x) {
			cc = sapply(names(x[["CC"]]), function (m) {
												tail(x[["CC"]][[m]], 1)[c("obs.cum", "mod.cum")]
												}
						)
			cc = as.data.frame(cc)
			return(cc)
			}

get.multi.cc = function (x) {
			xp = as.character(ls.str(x))
			cc = list()
			
			for (i in xp)
			cc[[i]] = sapply(names(x[[i]][["CC"]]), function (m) {
												tail(x[[i]][["CC"]][[m]], 1)[c("obs.cum", "mod.cum")]
												}
						)
			cc = as.data.frame(cc)
			return(cc)
			}
		
