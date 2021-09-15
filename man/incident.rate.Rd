\name{incident.rate}
\alias{incident.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OSHA Incident Rate Calculator
}
\description{
OSHA Incident Rate calculator
}
\usage{
ir(recordable_cases, total_labor_hours)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recordable_cases}{
%%     ~~Describe \code{recordable_cases} here~~
}
  \item{total_labor_hours}{
%%     ~~Describe \code{total_labor_hours} here~~
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
function (recordable_cases, total_labor_hours)
{
    recordable_cases * 2e+05/total_labor_hours
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
