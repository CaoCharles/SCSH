\name{plot_em}
\alias{plot_em}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_em(theta)
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
    r1 <- rmvnorm(73984, mean = theta$mu1, sigma = theta$sigma1)
    colnames(r1) <- c("D1x", "D1y")
    r2 <- rmvnorm(73984, mean = theta$mu2, sigma = theta$sigma2)
    colnames(r2) <- c("D2x", "D2y")
    r <- rbind(r1, r2)
    data1 <- rbind(data1, data1)
    data <- cbind(data1, r)
    A <- ggplot(data) + labs(title = " Waiting time vs Eruption time of old faithful geyser", 
        x = "Eruptions time (min)", y = "Time to next eruption (min)") + 
        xlim(0, 6) + ylim(40, 100) + geom_point(aes(x = eruptions, 
        y = waiting)) + stat_density2d(aes(x = D1x, y = D1y), 
        col = "red") + theme(title = element_text(face = "bold")) + 
        scale_fill_gradientn(colours = rainbow(9)) + theme(panel.background = element_rect(colour = "black", 
        size = 2))
    return(A)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
