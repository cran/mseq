\name{martPred}
\alias{martPred}
\title{ predict using MART }
\description{
 Get the predicted log sequencing preferences by the trained MART model,
 and adjust them so that read \code{TT....T} have value 0.
}
\usage{
martPred(train.gbm, newdata, n.trees = 2000)
}
\arguments{
  \item{train.gbm}{ the trained gbm object }
  \item{newdata}{ the data that we want to predict }
  \item{n.trees}{ number of trees to be considered }
}
\value{
 a numeric vector of predicted log preferences
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3) # here the surrounding sequences is only of length 5. In real datasets, it should be larger.
 
 # train and predict by MART
 train.data <- data[data$index < 6, ]
 test.data <- data[data$index >= 6, ]
 train.mart <- martTrain(train.data, interaction.depth = 2, n.trees = 100)
 pred.pref <- exp(martPred(train.mart, test.data, n.trees = 100))
}
\keyword{ models }
\keyword{ tree }
