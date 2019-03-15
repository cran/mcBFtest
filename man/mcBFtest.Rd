\name{mcBFtest}
\alias{mcBFtest}

\title{
Monte Carlo based tests as an alternative of Welch's t-approximation
}
\description{

In the t test, it is usually the case that the assumption of equal variances on the two groups is violated. The test problem is known as the Behrens-Fisher (BF) problem when
no assumption of equal population variances can be made. For the BF problem,
the T statistic provides value for a given dataset and its statistical
distribution is not easy to characterise.

To our knowledge,
the best approximation thus far is due to Welch (1938). The Welch’s test involves two
layers of approximations: approximating the distribution of the statistic by a t-distribution, which in turn
depends on an approximate degrees of freedom.

The Monte Carlo based tests improve upon Welch’s approximate
test by avoiding one layer of approximation, resulting in enhancement in statistical power than Welch's t-approximation.



 }
\usage{
mcBFtest(x, y, method, MC)
}

\arguments{
  \item{x}{a (non-empty) numeric vector of data values
}
  \item{y}{a (non-empty) numeric vector of data values
}
  \item{method}{if "t" is used, we will use t-test assuming equal variance and df=n+m-2. If "W" is used, we assume unequal variance and the Welch approximation is used. If "Monte Carlo", the Monte Carlo procedure is applied.
}
  \item{MC}{a number for Monte Carlo procedure
}

}
%\details{
%efgdgd
%}
\value{
The function returns a list including

\item{p.value}{ the p-value for the test.}

}
\references{

Welch, B.L. (1938). The significance of the difference between two means when the
population variances are unequal. Biometrika, 29 (3/4), 350–362.

Ullah, I., Paul, S., Hong, Z., & Wang, Y-G. (2019). Significance tests for analyzing gene expression data with small sample sizes. Bioinformatics, in press.


}
\author{
You-Gan Wang, Na Wang
}


\seealso{
\code{t.test} function from package \code{stats}
}
\examples{

library(mcBFtest)

x <- sleep[1:10,1]
y <- sleep[11:20,1]

mcBFtest(x, y, method = "t")
mcBFtest(x, y, method = "W")
mcBFtest(x, y, method = "Monte Carlo", MC = 100000)

}
\keyword{regression}
