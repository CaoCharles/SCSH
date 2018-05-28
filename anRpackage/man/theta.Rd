\name{theta}
\alias{theta}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("theta")}
\format{
  The format is:
List of 5
 $ wei   : num [1:2] 0.356 0.644
 $ mu1   : Named num [1:2] 2.04 54.48
  ..- attr(*, "names")= chr [1:2] "eruptions" "waiting"
 $ mu2   : Named num [1:2] 4.29 79.97
  ..- attr(*, "names")= chr [1:2] "eruptions" "waiting"
 $ sigma1: num [1:2, 1:2] 0.0699 0.44 0.44 34.0511
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "eruptions" "waiting"
  .. ..$ : chr [1:2] "eruptions" "waiting"
 $ sigma2: num [1:2, 1:2] 0.171 0.946 0.946 36.249
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "eruptions" "waiting"
  .. ..$ : chr [1:2] "eruptions" "waiting"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(theta)
## maybe str(theta) ; plot(theta) ...
}
\keyword{datasets}
