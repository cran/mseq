`getDev` <-
function(pred_count, real_count)
{
	dev <- sum(real_count[real_count != 0] * log(real_count[real_count != 0] / pred_count[real_count != 0]))
	dev <- dev - sum(real_count) + sum(pred_count)
	dev <- 2 * dev
	
	return(dev)
}

