\name{mseq-package}
\alias{mseq-package}
\alias{mseq}
\docType{package}
\title{
 Modeling non-uniformity in short-read rates in RNA-Seq data
}
\description{
 This package implements all the methods in the paper "Modeling non-uniformity in short-read rates in RNA-Seq data".
 Especially, it implements both the iterative glm procedure for the Poisson linear model and the training procedure of the MART model.
 The cross-validation for both of the methods is also implemented.
 Moreover, in folder \code{data_top100}, data files of the top 100 genes and surrounding sequences of all the eight sub-datasets are provided.
 In Version 1.1, the Poisson linear model with dinucleotide composition is also implemented.
}
\details{
\tabular{ll}{
Package: \tab mseq\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2010-01-27\cr
License: \tab GPL (version 2 or newer)\cr
LazyData: \tab yes\cr
}
 This package mainly includes the following functions:\cr
 \code{expData.R}: Expand the surrounding sequences in the original file.\cr
 \code{plotCounts.R}: Plot the counts in the original data or fitted counts.\cr
 \code{getDev.R}: Calculate the deviance under Poisson model.\cr
 \code{getNullCount.R}: Get the counts of reads for each position under the null hypothesis.\cr
 \code{getPredCount.R}: Get the predicted counts given the predicted preferences.\cr
 \code{iterGlm.R}: Fit the Poisson linear model by iteratively fitting glm and gene expression levels.\cr
 \code{plotCoef.R}: Plot the coefficients of Poisson linear model.\cr
 \code{glmPred.R}: Predict the log preferences using the trained iterative glm model.\cr
 \code{iterGlmCV.R}: Get the cross-validation R squared for iterative glm.\cr
 \code{martTrain.R}: Train the MART model.\cr
 \code{martPred.R}: Get the predicted log sequencing preferences by the trained MART model.\cr
 \code{martCV.R}: Get the cross-validation R squared for MART.\cr
 \code{expData2nt.R}: Expand the surrounding sequences in the original file, dinucleotide composition included.\cr
 \code{iterGlm2nt.R}: Fit the Poisson linear model with dinucleotide composition by iteratively fitting glm and gene expression levels.\cr
 \code{glmPred2nt.R}: Predict the log preferences using the trained iterative glm model with dinucleotide composition.\cr
 \code{iterGlmCV2nt.R}: Get the cross-validation R squared for iterative glm with dinucleotide composition.\cr
}
\author{
Jun Li

Maintainer: Jun Li <junli07@stanford.edu>
}
\references{
 Li J, Jiang H, Wong WH, Modeling non-uniformity in short-read rates in RNA-Seq data, submitted.
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3) # here the surrounding sequences is only of length 5. In real datasets, it should be larger.
 pdf("ori_counts.pdf")
 plotCounts(data$count[data$index == 1])
 dev.off()

 # get the CV R squared for Poisson linear model
 R_sq <- iterGlmCV(data)

 # train and predict by Poisson linear model
 train.data <- data[data$index < 6, ]
 test.data <- data[data$index >= 6, ]
 train.glm <- iterGlm(train.data)
 pdf("coef.pdf")
 plotCoef(train.glm, 2, 3)
 dev.off()
 pred.pref <- exp(glmPred(train.glm, test.data))

 # get predicted counts
 pred.count <- getPredCount(test.data, pred.pref)
 pdf("glm_fitted_counts.pdf")
 plotCounts(pred.count[data$index == 1])
 dev.off()

 # get the R squared
 glm.dev <- getDev(pred.count, test.data$count)
 null.count <- getNullCount(test.data)
 null.dev <- getDev(null.count, test.data$count)
 R_sq <- 1 - glm.dev / null.dev

 # To shorten the running time, this example uses small values of interaction.depth and n.trees. For real datasets, it is strongly suggested to use their default values.
 # get the CV R squared for MART
 R_sq <- martCV(data, interaction.depth = 2, n.trees = 100)

 # train and predict by MART
 train.data <- data[data$index < 6, ]
 test.data <- data[data$index >= 6, ]
 train.mart <- martTrain(train.data, interaction.depth = 2, n.trees = 100)
 pred.pref <- exp(martPred(train.mart, test.data, n.trees = 100))

 # get predicted counts
 pred.count <- getPredCount(test.data, pred.pref)
 pdf("mart_fitted_counts.pdf")
 plotCounts(pred.count[data$index == 1])
 dev.off()

 # get the R squared
 mart.dev <- getDev(pred.count, test.data$count)
 null.count <- getNullCount(test.data)
 null.dev <- getDev(null.count, test.data$count)
 R_sq <- 1 - mart.dev / null.dev
 
 
 #### Poissong linear model using dinucleotide composition
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData2nt(g1_part, 2, 1) # here the surrounding sequences is only of length 3. In real datasets, it should be larger.

 # get the CV R squared for Poisson linear model
 R_sq <- iterGlmCV2nt(data)

 # train and predict by Poisson linear model
 train.data <- data[data$index < 6, ]
 test.data <- data[data$index >= 6, ]
 train.glm <- iterGlm2nt(train.data)
 pred.pref <- exp(glmPred2nt(train.glm, test.data))
}
\keyword{ package }
\keyword{ models }
\keyword{ regression }
\keyword{ tree }
