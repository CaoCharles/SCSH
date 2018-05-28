\name{plot.em}
\alias{plot.em}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.em(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (theta) 
{
    mixture.contour <- outer(xpts, ypts, function(x, y) {
        theta$wei[1] * dmvnorm(cbind(x, y), mean = theta$mu1, 
            sigma = theta$sigma1) + theta$wei[2] * dmvnorm(cbind(x, 
            y), mean = theta$mu2, sigma = theta$sigma2)
    })
    contour(xpts, ypts, mixture.contour, nlevels = 8, drawlabel = FALSE, 
        col = "red", xlab = "Eruption time (mins)", ylab = "Waiting time (mins)", 
        main = "Waiting time vs Eruption time of the Old Faithful geyser")
    points(faithful)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
