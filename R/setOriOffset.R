`setOriOffset` <-
function(data)
{
	uniq_cat <- unique(data$index)
	log_expr <- rep(0, length(data$index))
	
	for (k in uniq_cat)
	{
		log_expr[data$index == k] <- log(sum(data$count[data$index == k]) / sum(data$index == k))
	}

	return(log_expr)
}

