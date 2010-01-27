`glmPred2nt` <-
function(train.glm, newdata)
{
	log_pref <- as.numeric(as.matrix(newdata[, -c(1, 2)]) %*% as.numeric(train.glm$coefficients[-1]))
	
	return(log_pref)
}

