\name{rwl}
\alias{rwl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recommended Weight Limit and Lifting Index
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rwl(horizontal.dist, vertical.dist, distance, angle, seconds.between.lifts, grasp, object.weight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{horizontal.dist}{
%%     ~~Describe \code{horizontal.dist} here~~
}
  \item{vertical.dist}{
%%     ~~Describe \code{vertical.dist} here~~
}
  \item{distance}{
%%     ~~Describe \code{distance} here~~
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
}
  \item{seconds.between.lifts}{
%%     ~~Describe \code{seconds.between.lifts} here~~
}
  \item{grasp}{
%%     ~~Describe \code{grasp} here~~
}
  \item{object.weight}{
%%     ~~Describe \code{object.weight} here~~
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
function (horizontal.dist, vertical.dist, distance, angle, seconds.between.lifts,
    grasp, object.weight)
{
    LC <- 23
    if (horizontal.dist <= 25) {
        HM <- 1
    }
    else if (horizontal.dist > 25 & horizontal.dist <= 30) {
        HM <- 0.83
    }
    else if (horizontal.dist > 30 & horizontal.dist <= 40) {
        HM <- 0.63
    }
    else if (horizontal.dist > 40 & horizontal.dist <= 50) {
        HM <- 0.5
    }
    else if (horizontal.dist > 50 & horizontal.dist <= 60) {
        HM <- 0.42
    }
    else print("Must be between 0 and 60 cm.")
    if (vertical.dist <= 30) {
        VM <- 0.78
    }
    else if (vertical.dist > 30 & vertical.dist <= 50) {
        VM <- 0.87
    }
    else if (vertical.dist > 50 & vertical.dist <= 70) {
        VM <- 0.93
    }
    else if (vertical.dist > 70 & vertical.dist <= 100) {
        VM <- 0.99
    }
    else if (vertical.dist > 100 & vertical.dist <= 150) {
        VM <- 0.93
    }
    else if (vertical.dist > 150 & vertical.dist <= 175) {
        VM <- 0.78
    }
    else if (vertical.dist == 175) {
        VM <- 0.7
    }
    else if (vertical.dist > 175) {
        VM <- 0
    }
    if (distance <= 25) {
        DM <- 1
    }
    else if (distance > 25 & distance <= 40) {
        DM <- 0.93
    }
    else if (distance > 40 & distance <= 55) {
        DM <- 0.9
    }
    else if (distance > 55 & distance <= 100) {
        DM <- 0.87
    }
    else if (distance > 100 & distance <= 145) {
        DM <- 0.85
    }
    else if (distance > 145 & distance <= 175) {
        DM <- 0.85
    }
    else if (distance == 175) {
        DM <- 0
    }
    if (angle == 90) {
        A <- 0.71
    }
    else if (angle == 60) {
        A <- 0.81
    }
    else if (angle == 45) {
        A <- 0.86
    }
    else if (angle == 30) {
        A <- 0.9
    }
    else if (angle == 0) {
        A <- 1
    }
    else print("Angle must be input as 90, 60, 45, 30, or 0 degrees.")
    if (seconds.between.lifts == 300) {
        FM <- 0.85
    }
    else if (seconds.between.lifts == 60) {
        FM <- 0.75
    }
    else if (seconds.between.lifts == 30) {
        FM <- 0.65
    }
    else if (seconds.between.lifts == 15) {
        FM <- 0.45
    }
    else if (seconds.between.lifts == 10) {
        FM <- 0.27
    }
    else if (seconds.between.lifts == 6) {
        FM <- 0.13
    }
    else print("Time must be entered as 300, 60, 30, 15, 10, or 6 seconds.")
    if (grasp == 1) {
        CM <- 1
    }
    else if (grasp == 0) {
        CM <- 0.9
    }
    else print("Use 1 for good and fair grasps, or 0 for poor grasp.")
    weight.limit <<- 23 * HM * VM * DM * FM * A * CM
    LI <<- object.weight/weight.limit
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
