`martCV` <-
function(data, fold = 5, seed = 281142, shrinkage = 0.06, interaction.depth = 10, n.trees = 2000, small_count = 0.5)
{
	cat("Doing cross-validation for MART...\n")
	train_index <- CVTrainIndexGene(data, fold, seed)
	weight <- getWeight(data)
	
	dev <- rep(0, fold)
	null_dev <- rep(0, fold)
	
	for (j in 1 : fold)
	{
		cat("\nFold", j, "...\n")

		log_pref <- getOriLogPref(data[train_index[j, ], ], small_count)
		
		train.gbm <- gbm(log_pref ~ . - count - index, distribution = "gaussian", data = data[train_index[j, ], ], weights = weight[train_index[j, ]], n.trees = n.trees, shrinkage = shrinkage, interaction.depth = interaction.depth)

		dev[j] <- getDev(getPredCount(data[!train_index[j, ], ], exp(martPred(train.gbm, data[!train_index[j, ], ], n.trees))), data[!train_index[j, ], ]$count)
		null_dev[j] <- getDev(getNullCount(data[!train_index[j, ], ]), data[!train_index[j, ], ]$count)
		
		cat("R_squared for fold", j, "=", 1 - dev[j] / null_dev[j], "\n")
	}
	
	cat("deviance =", dev, "\n")
	cat("null deviance =", null_dev, "\n")
	
	R_squared <- 1 - sum(dev) / sum(null_dev)
	cat("R_squared =", R_squared, "\n")
	
	return(R_squared)
}

