\name{mgm3.to.ppm}
\alias{mgm3.to.ppm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert mg/m3 to ppm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mgm3.to.ppm(mg.per.cubic.meter, molecular.weight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mg.per.cubic.meter}{
%%     ~~Describe \code{mg.per.cubic.meter} here~~
}
  \item{molecular.weight}{
%%     ~~Describe \code{molecular.weight} here~~
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
function (mg.per.cubic.meter, molecular.weight)
{
    ppm <<- mg.per.cubic.meter * 24.45/molecular.weigth
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
