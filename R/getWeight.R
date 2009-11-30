`getWeight` <-
function(data)
{
	cat("Calculating the weight...\n")
	uniq_cat <- unique(data$index)
	weight <- rep(0, length(data$index))
	for (k in uniq_cat)
	{
		weight[data$index == k] <- sum(data$count[data$index == k]) / sum(data$index == k)
	}
	return(weight)
}

