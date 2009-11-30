`updateOffset` <-
function(data, pred_count, log_expr)
{
	uniq_cat <- unique(data$index)
	
	log_pref <- log(pred_count) - log_expr
	
	for (k in uniq_cat)
	{
		log_expr[data$index == k] <- log(sum(data$count[data$index == k]) / sum(exp(log_pref[data$index == k])))
	}

	return(log_expr)
}

