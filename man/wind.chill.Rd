\name{wind.chill}
\alias{wind.chill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wind Chill Calculator
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wind.chill(temp.fahr, wind.speed.mph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp.fahr}{
%%     ~~Describe \code{temp.fahr} here~~
}
  \item{wind.speed.mph}{
%%     ~~Describe \code{wind.speed.mph} here~~
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
function (temp.fahr, wind.speed.mph)
{
    if (temp.fahr < 70) {
        temperature <<- 35.74 + (0.6215 * temp.fahr) - (35.75 *
            (wind.speed.mph^0.16)) + (0.4275 * (temp.fahr * (wind.speed.mph^0.16)))
        sprintf("Wind chill is \%.2f F when wind speed is \%.1f and air temperature is \%.1f F",
            temperature, wind.speed.mph, temp.fahr)
    }
    else {
        print("Air temperature needs to be below 70 F")
    }
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
