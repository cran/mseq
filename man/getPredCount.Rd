\name{getPredCount}
\alias{getPredCount}
\title{ predicted counts }
\description{
 Get the predicted counts given the predicted preferences.
}
\usage{
getPredCount(data, pred_pref)
}
\arguments{
  \item{data}{ the data frame generated by expData.R }
  \item{pred_pref}{ the predicted preferences }
}
\value{
 predicted counts, a numeric vector
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3) # In real datasets, surrounding sequences should be set longer.
 
 # train and predict by Poisson linear model
 train.data <- data[data$index < 6, ]
 test.data <- data[data$index >= 6, ]
 train.glm <- iterGlm(train.data)
 pred.pref <- exp(glmPred(train.glm, test.data))

 pred.count <- getPredCount(test.data, pred.pref)
}
\keyword{ models }
