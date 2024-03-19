\name{rmse_matrix}
\alias{rmse_matrix}

\title{ root mean squared error (RMSE) matrix between given variables
}
\description{
Calculates RMSE, \eqn{\sqrt{\sum\frac{(x_1 - x_2)^2}{n}}}, between all of the unique combinations of the supplied variables (at least 2 variables required). The RMSEs are retuned in matrix form (lower triangular), similar to a correlation matrix.
}
\usage{
rmse_matrix(dat,
            scale = FALSE,
            labels = FALSE)
}

\arguments{
  \item{dat}{ a matrix or data.frame object containing the variables among which one whishes to calculate RMSEs
}
\item{scale}{ if TRUE, the variables are stndardized before calculating the RMSEs
}
\item{labels}{ if TRUE, the resulting columns/row of the matrix of RMSEs will have variable labels
}
}

\value{
a matrix containing RMSEs between input variables (only lower triangle is filled out)
}

\author{
Fabio Setti
}

\examples{

x <- cbind(rep(1:4, 4),
           rep(5:8, 4),
           rep(9:12, 4))


colnames(x) <- c("x1", "x2", "x3")

rmse_matrix(x, labels = TRUE)

}

