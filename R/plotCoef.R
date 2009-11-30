plotCoef <- function(data.glm, llen, rlen)
{
	# get the coefficients
	coef <- data.glm$coefficients[-1]
	p <- length(coef) / 3
	coef_T <- rep(0, p)
	coef_A <- coef[c(1 : p) * 3 - 2]
	coef_C <- coef[c(1 : p) * 3 - 1]
	coef_G <- coef[c(1 : p) * 3]

	# get the positions
	pos <- c((-llen) : (rlen - 1))

	plot(NA, NA, xlim = c(min(pos), max(pos)), ylim = 1.0 * c(min(coef), max(coef)), xlab = "position", ylab = "coefficients")
	title("coefficients, red-T, green-A, blue-C, black-G")
	
	lines(pos, coef_T, type = 'o', col = 'red', cex = 0.5)
	lines(pos, coef_A, type = 'o', col = 'green', cex = 0.5)
	lines(pos, coef_C, type = 'o', col = 'blue', cex = 0.5)
	lines(pos, coef_G, type = 'o', col = 'black', cex = 0.5)
}
