\name{irf_stukel.2PL}
\alias{irf_stukel.2PL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 2PL Sigmoid probability function based on Stukel generalized logistic model (1988)
}
\description{
2PL Sigmoid probability funciton based on Stukel generalized logistic model (1988). The "2PL" part stands for the the two IRT parameters known as discrimination, a, and difficulty, b. This function incorporates 4 items parameters (a, b, alpha1 ,and alpha2) and 1 person parameter (theta). This IRT model can generate both symmetric and asymmetric item respone functions.
}
\usage{
irf_stukel.2PL(theta, a = 1, b = 0, alpha1 = 0, alpha2 = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{Latent trait person parameter}
  \item{a}{ Item discrimination}
  \item{b}{ Item difficulty}
  \item{alpha1}{ Upper tail shape parameter}
  \item{alpha2}{ Lower tail shape parameter}
  }
\value{returns a probability between 0 and 1}
\references{
Stukel, T. A. (1988). Generalized logistic models. Journal of the American Statistical Association, 83 (402), 426-431.
}
\author{
Fabio Setti
}
