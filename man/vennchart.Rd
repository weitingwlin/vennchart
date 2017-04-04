\name{vennchart}
\alias{vennchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Making Venn/Eular charts
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vennchart(A_all, B_all, C_all, 
          AandB, AandC, BandC)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A_all, B_all, C_all}{Values of variable A, B and C. The area of circles corresponds to these values.
%%     ~~Describe \code{x} here~~
}
  \item{AandB, AandC, BandC}{ Value of intersections between two variables. The values correspond to the ovelaping circle area. Note that the overlaping area of three variables are not considered in rendering the plot.
  }
  \item{colors}{List of 3 color arguments, each item correpond to one variable, and will be passed to the polygon\{graphics\} function.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{circles }{A list of centers (\code{c1, c2, c3}) and radius (\code{r1, r2, r3}) of the three circles.}
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
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
