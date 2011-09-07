\name{expData2nt}
\alias{expData2nt}
\title{ expand the surrounding sequences, including both the single nucleotide and dinucleotide composition }
\description{
 The original data file only contains one sequence. This function will 
 expand for each retained position its surrounding sequence, including
 both the single nucleotide and dinucleotide composition. The output
 data frame can be directly used by Poisson linear model and MART.
}
\usage{
expData2nt(oriData, llen, rlen)
}
\arguments{
  \item{oriData}{ the original data frame read directly from the file }
  \item{llen}{ the number of nucleotides before the first nucleotide of a read, which we consider as surrounding sequence }
  \item{rlen}{ the number of nucleotides in and after the first nucleotide of a read, which we consider as surrounding sequence }
}
\details{
 Note that we will not check the format of \code{oriData} here. Please generate the original data file carefully.
 Please refer to \code{Readme_format.txt} for the details of the format.
}
\value{
 a data frame including the counts and surrounding sequences, which can be directly used by Poisson linear model and MART.
 The number of rows are the number of \code{0}s in \code{oriData$tag}. The number of columns are \code{2 + (llen + rlen) * 3 + (llen + rlen - 1) * 9}, with names 
 \code{index, count, pMllen.A, pMllen.C, pMllen.G, ..., (prlen-1).A, (prlen-1).C, (prlen-1).G, pMllen.pM(llen-1).TA, ..., p(rlen-2).p(rlen-1).CC}, where \code{p} means position, \code{M} means minus.
}
\examples{
 # read and expand the data
 data(g1_part) # for real data, please use read.csv, like g1 <- read.csv("g1.csv")
 data <- expData2nt(g1_part, 2, 1) # In real datasets, the surrounding sequences should be set longer.
}
\keyword{ models }
