`getPredCount` <-
function(data, pred_pref)
{
	uniq_cat <- unique(data$index)
	pred_count <- rep(0, length(data$count))
	for (k in uniq_cat)
	{
		pred_count[data$index == k] <- sum(data$count[data$index == k]) / sum(pred_pref[data$index == k]) * pred_pref[data$index == k]
	}
	return(pred_count)
}

