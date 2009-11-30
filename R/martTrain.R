`martTrain` <-
function(data, shrinkage = 0.06, interaction.depth = 10, n.trees = 2000, small_count = 0.5)
{
	cat("Training the data using Gbm...\n")
	weight <- getWeight(data)
	
	log_pref <- getOriLogPref(data, small_count)
	
	train.gbm <- gbm(log_pref ~ . - count - index, distribution = "gaussian", data = data, weights = weight, n.trees = n.trees, shrinkage = shrinkage, interaction.depth = interaction.depth)
	
	null_dev <- getDev(getNullCount(data), data$count)
	dev <- getDev(getPredCount(data, exp(martPred(train.gbm, data, n.trees))), data$count)
	cat("deviance =", dev, "\n")
	cat("null deviance =", null_dev, "\n")
	cat("R_squared =", 1 - sum(dev) / sum(null_dev), "\n")
	
	return(train.gbm)
}

