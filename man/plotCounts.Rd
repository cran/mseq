\name{plotCounts}
\alias{plotCounts}
\title{ plot counts }
\description{
 This function creates a bar-plot like image. So it can be used to plot counts or sequencing preferences.
}
\usage{
plotCounts(counts)
}
\arguments{
  \item{counts}{ a sequence of positive values }
}
\value{
 No returned value. Will generate a plot.
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3)

 pdf("plot_counts.pdf")
 plotCounts(data$count[data$index == 1])
 dev.off()
}
\keyword{ models }
