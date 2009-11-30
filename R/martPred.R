`martPred` <-
function(train.gbm, newdata, n.trees = 2000)
{
	# original prediction
	pred <- predict.gbm(train.gbm, newdata, n.trees)
	
	# construct all T data and get its prediction
	T_data <- newdata[1, ]
	T_data[1, 3 : dim(newdata)[2]] <- 'T'
	for (j in 3 : dim(newdata)[2])
	{
		T_data[, j] <- factor(T_data[, j], levels = c('T', 'A', 'C', 'G'))
	}
	pred_T <- predict.gbm(train.gbm, T_data, n.trees)
	
	# modify the prediction
	pred <- pred - pred_T
	
	return(pred)
}

