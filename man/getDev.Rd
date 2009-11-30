\name{getDev}
\alias{getDev}
\title{ Poisson deviance }
\description{
 Calculate the deviance under Poisson model.
}
\usage{
getDev(pred_count, real_count)
}
\arguments{
  \item{pred_count}{ the fitted or predicted counts }
  \item{real_count}{ the real counts in the data }
}
\value{
 the deviance, a single numeric value
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3)

 null.count <- getNullCount(data)
 null.dev <- getDev(null.count, data$count)
}
\references{ Hardin JW, Hilbe JM: Generalized Linear Models and Extensions. 2 edn. College Station, TX: Stata Press; 2007. }
\keyword{ models }
