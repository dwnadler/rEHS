\name{concentration}
\alias{concentration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate New Concentration after a Time Interval
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
concentration(contaminant.generation.rate, flow.rate.cfm, timeframe, volume.cf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contaminant.generation.rate}{
%%     ~~Describe \code{contaminant.generation.rate} here~~
}
  \item{flow.rate.cfm}{
%%     ~~Describe \code{flow.rate.cfm} here~~
}
  \item{timeframe}{
%%     ~~Describe \code{timeframe} here~~
}
  \item{volume.cf}{
%%     ~~Describe \code{volume.cf} here~~
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
function (contaminant.generation.rate, flow.rate.cfm, timeframe,
    volume.cf)
{
    a <- exp(-1 * flow.rate.cfm * timeframe/volume.cf)
    numerator <- contaminant.generation.rate * (1 - a)
    conc <<- numerator * 1e+06/flow.rate.cfm
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
