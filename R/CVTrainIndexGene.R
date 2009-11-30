`CVTrainIndexGene` <-
function(data, fold, seed)
{
	uniq_cat <- unique(data$index)
	train_index <- matrix(TRUE, fold, dim(data)[1])
	
	set.seed(seed)
	cv_cat <- sample(uniq_cat)
	for (j in 1 : length(uniq_cat))
	{
		row_num <- ceiling(j / floor(length(uniq_cat) / fold))
		if (row_num > fold)
		{
			row_num <- fold
		}
		train_index[row_num, data$index == cv_cat[j]] <- FALSE
	}
	
	return(train_index)
}

