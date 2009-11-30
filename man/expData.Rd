\name{expData}
\alias{expData}
\title{ expand the surrounding sequences }
\description{
 The original data file only contains one sequence. This function will 
 expand for each retained position its surrounding sequence. The output
 data frame can be directly used by Poisson linear model and MART.
}
\usage{
expData(oriData, llen, rlen)
}
\arguments{
  \item{oriData}{ the original data frame read directly from the file }
  \item{llen}{ the number of nucleotides before the first nucleotide of a read, which we consider as surrounding sequence }
  \item{rlen}{ the number of nucleotides in and after the first nucleotide of a read, which we consider as surrounding sequence }
}
\details{
 Note that we will not check the format of \code{oriData} here. Please generate the original data file carefully.
 Please refer to \code{Readme_format.txt} in folder \code{data_top100} for the details of the format.
}
\value{
 a data frame including the counts and surrounding sequences, which can be directly used by Poisson linear model and MART.
 The number of rows are the number of \code{0}s in \code{oriData$tag}. The number of columns are \code{llen + rlen + 2}, with names 
 \code{index, count, pMllen, ..., pM1, p0, p1, ..., prlen}, where \code{p} means position, \code{M} means minus.
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData(g1_part, 2, 3) # In real datasets, the surrounding sequences should be set longer.
}
\keyword{ models }
