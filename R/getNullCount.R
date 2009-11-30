`getNullCount` <-
function(data)
{
	uniq_cat <- unique(data$index)
	null_count <- rep(0, length(data$count))
	for (k in uniq_cat)
	{
		null_count[data$index == k] <- sum(data$count[data$index == k]) / sum(data$index == k)
	}
	
	return(null_count)
}

