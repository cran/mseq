`expData2nt` <-
function(oriData, llen, rlen)
{
	cat("normal expanding...\n")
	data <- expData(oriData, llen, rlen)
	
	c <- llen + rlen
	### the numeric data
	cat("Getting the numberic data...\n")
	data.num <- matrix(0, dim(data)[1], c)
	for (i in 1 : c)
	{
		data.num[, i] <- as.numeric(data[, 2 + i])
	}
	
	### the single nucleotide numeric data
	cat("Getting single nucleotide numeric data...\n")
	data.1d <- matrix(0, dim(data)[1], 4 * c)
	for (i in 1 : c)
	{
		for (j in 1 : 4)
		{
			data.1d[data.num[, i] == j, 4 * (i - 1) + j] <- 1
		}
	}
	
	### the bi-nucleotide numeric data
	cat("Getting bi-nucleotide numeric data...\n")
	data.2d <- matrix(0, dim(data)[1], 16 * (c - 1))
	for (i in 1 : (c - 1))
	{
		for (j in 1 : 4)
		{
			for (k in 1 : 4)
			{
				data.2d[(data.1d[, (i - 1) * 4 + j] == 1) & (data.1d[, i * 4 + k] == 1), (i - 1) * 16 + 4 * (j - 1) + k] <- 1
			}
		}
	}
	
	### delete the unnecessary columns
	cat("Deleting unnecessary columns...\n")
	data.1d <- data.1d[, c(1 : (4 * c)) %% 4 != 1]
	data.2d <- data.2d[, (c(1 : (16 * (c - 1))) %% 16 != 1) & (c(1 : (16 * (c - 1))) %% 16 != 8) & (c(1 : (16 * (c - 1))) %% 16 != 0) & (c(1 : (16 * (c - 1))) %% 16 < 12)]
	
	cat("Number of columns in data.1d = ", dim(data.1d)[2], "\n", sep = '')
	cat("Number of columns in data.2d = ", dim(data.2d)[2], "\n", sep = '')
	
	### combine them
	cat("Combining the single and bi nucleotide numeric data...\n")
	data.f <- data.frame(index = data$index, count = data$count, data.1d, data.2d)
	
	### set the column names
	cname <- character(2 + c * 3 + (c - 1) * 9)
	cname[1] <- "index"
	cname[2] <- "count"
	
	char1 <- c('A', 'C', 'G')
	for (k in 1 : 3)
	{
		for (i in 1 : c)
		{
			j <- i - llen - 1
			if (j < 0)
			{
				cname[2 + (i - 1) * 3 + k] <- paste("pM", -j, '.', char1[k], sep = '')
			}
			else
			{
				cname[2 + (i - 1) * 3 + k] <- paste("p", j, '.', char1[k], sep = '')
			}
		}
	}
	
	char2 <- c('TA', 'TC', 'TG', 'AT', 'AA', 'AC', 'CT', 'CA', 'CC')
	for (k in 1 : 9)
	{
		for (i in 1 : (c - 1))
		{
			j <- i - llen - 1
			if (j < -1)
			{
				cname[2 + 3 * c + (i - 1) * 9 + k] <- paste("pM", -j, '.', "pM", -(j + 1), '.', char2[k], sep = '')
			}
			if (j > -1)
			{
				cname[2 + 3 * c + (i - 1) * 9 + k] <- paste("p", j, '.', "p", j + 1, '.', char2[k], sep = '')
			}
			if (j == -1)
			{
				cname[2 + 3 * c + (i - 1) * 9 + k] <- paste('pM1.p0.', char2[k], sep = '')
			}
		}
	}
	
	colnames(data.f) <- cname

	cat("Number of columns in data.f = ", dim(data.f)[2], "\n", sep = '')
	
	return(data.f)
}

