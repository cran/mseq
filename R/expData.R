`expData` <-
function(oriData, llen, rlen)
{
	cat("\nExpanding the surrounding sequences to get the data frame for formulas to use...\n")
	
	seq <- as.character(oriData$seq)

	run_i <- 0
	num_i <- sum(oriData$tag == 0)
	index <- numeric(num_i)
	sseq <- character(num_i * (llen + rlen))
	count <- numeric(num_i)
	for (i in 1 : length(seq))
	{
		if (oriData$tag[i] == 0)
		{
			run_i <- run_i + 1
			index[run_i] <- oriData$index[i]
			sseq[((run_i - 1) * (llen + rlen) + 1) : (run_i * (llen + rlen))] <- seq[(i - llen) : (i + rlen - 1)]
			count[run_i] <- oriData$count[i]
		}
	}

	sseq <- factor(sseq, levels = c('T', 'A', 'C', 'G'))
	sseq <- matrix(sseq, ncol = llen + rlen, byrow = TRUE)
	data <- data.frame(index = index, count = count, sseq)
	cname <- character(2 + llen + rlen)
	cname[1] <- "index"
	cname[2] <- "count"
	for (i in 3 : (2 + llen + rlen))
	{
		j <- i - 3 - llen
		if (j < 0)
		{
			cname[i] <- paste("pM", -j, sep = '')
		}
		else
		{
			cname[i] <- paste("p", j, sep = '')
		}
	}
	colnames(data) <- cname

	cat("number of genes =", length(unique(data$index)), "\n")
	cat("length of surrounding sequences =", dim(data)[2] - 2, "\n")
	cat("number of counts (positions) =", dim(data)[1], "\n")
	cat("total number of reads =", sum(data$count), "\n")

	return(data)
}

