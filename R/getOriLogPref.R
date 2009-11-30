`getOriLogPref` <-
function(data, small_count = 0.5)
{
	cat("Calculating the original log preferences...\n")
	
	modi_count <- data$count
	modi_count[data$count == 0] <- small_count
	
	uniq_cat <- unique(data$index)
	log_pref <- rep(0, length(data$index))
	
	for (k in uniq_cat)
	{
		mean_k <- sum(data$count[data$index == k]) / sum(data$index == k)
		log_pref[data$index == k] <- log(modi_count[data$index == k] / mean_k)
	}
	
	return(log_pref)
}

