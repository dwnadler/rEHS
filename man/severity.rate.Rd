\name{severity.rate}
\alias{severity.rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Severity Rate Calculator
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
severity.rate(total.days.lost, total.hours.worked)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{total.days.lost}{
%%     ~~Describe \code{total.days.lost} here~~
}
  \item{total.hours.worked}{
%%     ~~Describe \code{total.hours.worked} here~~
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
function (total.days.lost, total.hours.worked)
{
    total.days.lost * 2e+05/total.hours.worked
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
