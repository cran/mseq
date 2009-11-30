`iterGlm` <-
function(data, thrd = 0.01, max_iter = 10)
{
	cat("Begin iterative glm...\n")
	
	fordev <- 0
	log_expr <- setOriOffset(data)
	
	for (k in 1 : max_iter)
	{
		cat("\nIteration", k, "...\n")
		data.glm <- glm(count ~ . - index, data = data, family = poisson(link = "log"), offset = log_expr)
		log_expr <- updateOffset(data, data.glm$fitted.values, log_expr)
			
		cat("coefficients = ", data.glm$coefficients, "\n")
		nowdev <- getDev(data.glm$fitted.values, data$count)
		cat("deviance = ", nowdev, "\n")
		if (k > 1 && abs((nowdev - fordev) / fordev) < thrd)
		{
			break
		}
		fordev <- nowdev
	}

	dev <- getDev(data.glm$fitted.values, data$count)
	null_dev <- getDev(getNullCount(data), data$count)
	cat("deviance =", dev, "\n")
	cat("null deviance =", null_dev, "\n")
	cat("R_squared =", 1 - sum(dev) / sum(null_dev), "\n")
	
	return(data.glm)
}

