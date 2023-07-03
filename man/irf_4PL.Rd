\name{irf_4PL}
\alias{irf_4PL}

\title{ Probability distribution for 4PL IRT model
}
\description{
Calculates the probabilty of a keyed response (1, correct, insteasd of 0, wrong) given \eqn{\theta}, latent trait ability, \emph{a}, item discrimination, \emph{b}, item difficulty, \emph{c}, item guessing, and \emph{d}, item slipping.
}
\usage{
irf_4PL(theta, a = 1,  b = 0, c = 0, d = 1)
}

\arguments{
  \item{theta:}{Latent trait representitng the person level of the construct being measured}
  \item{a:}{Item discrimination}
  \item{b:}{ Item difficulty }
  \item{c:}{ Item guessing (sets a lower asymptote)}
  \item{d:}{ Item slipping (sets a upper asymptote)}
}

\value{ Probability ranging from 0 to 1}

\author{
Fabio Setti
}

