\name{glmPred}
\alias{glmPred}
\title{ predict using iterative glm }
\description{
 Predict the log preferences using the trained iterative glm model.
}
\usage{
glmPred(train.glm, newdata)
}
\arguments{
  \item{train.glm}{ the trained iterative glm model }
  \item{newdata}{ the new data }
}
\value{
 the predicted log preferences. a numeric vector.
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
}
\keyword{ models }
\keyword{ regression }
