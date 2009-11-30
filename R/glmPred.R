`glmPred` <-
function(train.glm, newdata)
{
	#### get the coefficients, 4 for a position ####
	oriCoef <- train.glm$coefficients[-1]
	p <- length(oriCoef) / 3
	coef <- rep(0, p * 4)
	for (i in 1 : p)
	{
		coef[(i - 1) * 4 + 2 : 4] <- oriCoef[(i - 1) * 3 + 1 : 3]
	}

	#### get the predicted log preferences ####
	log_pref <- rep(0, dim(newdata)[1])
	for (i in 1 : p)
	{
		log_pref <- log_pref + coef[(i - 1) * 4 + as.numeric(newdata[, i + 2])]
	}

	return(log_pref)
}

