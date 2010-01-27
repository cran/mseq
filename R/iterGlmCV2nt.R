`iterGlmCV2nt` <-
function(data, fold = 5, seed = 281142, thrd = 0.01, max_iter = 10)
{
	cat("Cross validation using iterative glm...\n")
	train_index <- CVTrainIndexGene(data, fold, seed)
	
	dev <- rep(0, fold)
	null_dev <- rep(0, fold)
	
	for (j in 1 : fold)
	{
		cat("\nFold", j, "...\n")

		log_expr <- setOriOffset(data[train_index[j, ], ])
		fordev <- 0
		for (k in 1 : max_iter)
		{
			cat("\nIteration", k, "...\n")
			train.glm <- glm(count ~ . - index, data = data[train_index[j, ], ], family = poisson(link = "log"), offset = log_expr)
			log_expr <- updateOffset(data[train_index[j, ], ], train.glm$fitted.values, log_expr)
			
			cat("coefficients = ", train.glm$coefficients, "\n")
			nowdev <- getDev(getPredCount(data[train_index[j, ], ], exp(glmPred2nt(train.glm, data[train_index[j, ], ]))), data[train_index[j, ], ]$count)
			cat("deviance = ", nowdev, "\n")
			if (k > 1 && abs((nowdev - fordev) / fordev) < thrd)
			{
				break
			}
			fordev <- nowdev
		}

		dev[j] <- getDev(getPredCount(data[!train_index[j, ], ], exp(glmPred2nt(train.glm, data[!train_index[j, ], ]))), data[!train_index[j, ], ]$count)
		null_dev[j] <- getDev(getNullCount(data[!train_index[j, ], ]), data[!train_index[j, ], ]$count)
		cat("\nFold ", j, ": null deviance = ", null_dev[j], "; deviance = ", dev[j], "; R squared = ", 1 - dev[j] / null_dev[j], "\n", sep = '')
	}
	
	cat("\n")
	cat("deviance =", dev, "\n")
	cat("null deviance =", null_dev, "\n")
	
	R_squared <- 1 - sum(dev) / sum(null_dev)
	cat("R_squared =", R_squared, "\n")
	
	return(R_squared)
}

