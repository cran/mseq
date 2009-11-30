plotCounts <- function(counts)
{
	plot(c(1, length(counts)), c(0, 0), type = 'l', ylim = c(0, max(counts)), col = 'white', xlab = "position", ylab = "counts")
	
	for (k in 1 : length(counts))
	{
		if (counts[k] != 0)
		{
			lines(c(k, k), c(0, counts[k]), col = 'black')
		}
	}
}

