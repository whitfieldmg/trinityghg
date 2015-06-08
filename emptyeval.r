# Make an empty EVAL dataframe

make.empty.eval = function() {
				empty = data.frame(Value = 1:15,
								   row.names = c("LOFIT", "F (MSLOFIT / MSE)", "F (critical at 5%)",
											   "RMSE", "RMSE (95% conf. limit",
											   "Relative error (E)", "E (95% conf. limit)",
											   "Mean difference (M)", "Student's t of M", "t-value (two-tailed)",
											   "Correlation coefficient (r)", "F-value", "F-value at p = 0.05",
											   "Obs", "Reps"))
				empty$Value = NA
				return(empty)
			}
			