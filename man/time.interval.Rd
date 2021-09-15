\name{time.interval}
\alias{time.interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Time Interval in Minutes for New Concentration of Contaminant
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
time.interval(volume.cf, flow.rate.cfm, contaminant.generation.rate.cfm, concentration.ppm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{volume.cf}{
%%     ~~Describe \code{volume.cf} here~~
}
  \item{flow.rate.cfm}{
%%     ~~Describe \code{flow.rate.cfm} here~~
}
  \item{contaminant.generation.rate.cfm}{
%%     ~~Describe \code{contaminant.generation.rate.cfm} here~~
}
  \item{concentration.ppm}{
%%     ~~Describe \code{concentration.ppm} here~~
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
function (volume.cf, flow.rate.cfm, contaminant.generation.rate.cfm,
    concentration.ppm)
{
    new.concentration <- concentration.ppm/1e+06
    a <- volume.cf/flow.rate.cfm
    b <- contaminant.generation.rate.cfm - flow.rate.cfm * new.concentration
    c <- log(b/contaminant.generation.rate.cfm)
    delta.t.min <<- -1 * a * c
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
