\name{plotCoef}
\alias{plotCoef}
\title{ plot coefficients of Poisson linear model }
\description{
 Plot the coefficients of Poisson linear model.
}
\usage{
plotCoef(data.glm, llen, rlen)
}
\arguments{
  \item{data.glm}{ the glm model trained by \code{iterGlm} }
  \item{llen}{ the number of nucleotides before the first nucleotide of a read, which we consider as surrounding sequence }
  \item{rlen}{ the number of nucleotides in and after the first nucleotide of a read, which we consider as surrounding sequence }
}
\value{
 No returned value. Will generate a plot.
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3)

 data.glm <- iterGlm(data)
 pdf("plot_coef.pdf")
 plotCoef(data.glm, 2, 3)
 dev.off()
}
\keyword{ models }
