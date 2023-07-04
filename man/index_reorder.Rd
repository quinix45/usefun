\name{index_reorder}

\alias{index_reorder}

\title{ Reorder vector or rows of matrix/data frame based on supply indices
}
\description{
This function reorder either a vector or the rows of a matrix/data frame according to the values a vector or matrix/data frame with the same dimensions (see example)
}
\usage{
index_reorder(original,
              order)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{original:}{orignal vector or matrix/data frame to be reordered
}
 \item{order:}{ vector or matrix/data frame with same dimensions as the "oringial" argument containing the order in which the vector or each row of the matrix/ data frame should be rearranged
}
}

\value{
A matrix with same dimensions as the "orignal" argument but with rows rearranged according to the "order" object
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fabio Setti
}

\examples{

# orignal matrix of values

x <- rbind(c( 3, 2, 6, 9),
           c( 3, 2, 6, 9),
           c( 3, 2, 6, 9))

# matrix with new order for every row

y <- rbind(c( 3, 2, 1, 4),
           c( 2, 3, 4, 1),
           c( 1, 4, 3, 2))

index_reorder(x,y)

}

