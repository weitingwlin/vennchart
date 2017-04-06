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
vennChart(A_all , B_all , C_all ,
          AandB , AandC , BandC,
          plot = TRUE, 
          colors = list(rgb(0,0,1,0.3), rgb(1,0, 0, 0.3), rgb(0, 1, 0, 0.3)),
          frameadj = 1.5, line  = NA, lwd = 2, 
          texts = TRUE, names = c("A", "B", "C"),
          Value = TRUE, cex = 1, textadj = c(0.5, 0.5),
          connect = "\n", unit = " ")

plotVenn(V, 
         color = list(rgb(0,0,1,0.3), rgb(1,0, 0, 0.3), rgb(0, 1, 0, 0.3)),
         line  = NA,lwd = 2, frameadj = 1.5)
plotVennTexts(P, Atext = "A", Btext = "B", Ctext = "C", 
              ABtext = " ", ACtext = " ", BCtext = " ",
              Value = TRUE, cex = 1, connect = "\n", unit = " ")
          
          

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A_all, B_all, C_all}{Values of variable A, B and C. The area of circles corresponds to these values.
%%     ~~Describe \code{x} here~~
}
  \item{AandB, AandC, BandC}{ Value of intersections between two variables. The values correspond to the ovelaping circle area. Note that the overlaping area of three variables are not considered in rendering the plot.
  }
  \item{plot}{Whether to make the plot (default: TRUE)}
  \item{colors}{List of 3 color arguments, each item correspond to one variable, and will be passed to the polygon\{graphics\} function.}
  \item{frameadj}{The size of the frame realtive to the minimal suqure to fit the graph.(Default : 1.5)}
  
  \item{lim}{A list of two arrays, passing to function \code{plot} as \code{xlim} and \code{ylim}. Default is \code{"default"}, that means calculat x, y limits from the \code{frameadj} value. This argument will overwrite the \code{frameadj} argument.}

  \item{line}{Eadge lines to the circles, the argyment will be passed to the \code{polygon\{graphics\}} function. Default is \code{NA} (no lines).}
    \item{lwd}{Eadge line width of the circles, the argyment will be passed to the \code{polygon\{graphics\}} function. Default is 2 (but will be ignored when \code{line = NA}).}
    
  \item{texts}{Whether to print texts on the plot. Default is TRUE.}
  \item{names}{An array of strings, names given to the three circles (variables).}
  \item{value}{whether to add variable values (\code{A_all, B_all}, ...) to the printed texts. Default is TRUE.}
  \item{cex}{Relative text size, will be passed to the \code{text\{graphics\}} function. Default is 1.}
  \item{textadj}{An array of 2 numerics.The locations of text (variables and overlapped area), as distance from a center in the unit of the radius. Default is \code{c(0.5, 0.5)}. }
  \item{connect}{A string. Attached between the variable names and values if values are printed. Default is \code{"\n"} (print value in a new line).}
  \item{unit}{A string. Attached to the end of the variable value if values are printed. Default is \code{" "}}
  \item{V}{The item \code{"circle"} from the output of \code{vennChart()}. See example.}
  \item{P}{The item \code{"textspos"} from the output of \code{vennChart()}. See example.}
  \item{Atext, Btext, Ctext}{Strings, the names of vairiables. (As the elements in the \code{names} argument.)}
  \item{ABtext, ACtext, BCtext}{Strings of the overlaping area.}
  
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{circles}{A list of centers (\code{c1, c2, c3}) and radius (\code{r1, r2, r3}) of the three circles.}
  \item{textpos}{Alist of calculated text positions.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
Set the output format to be square so the circles look right. In .Rmd notebooks, use \code{fig.width = s, fig.height = s} to make the output figure square.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# the vairiable values

  co_A <- 10
  co_B <- 12
  co_C <- 3
  co_AB <- 1.9 # where AB overlapped 
  co_BC <- 0.5
  co_AC <-1

# make the plot with default 

  vennChart(A_all = co_A, B_all = co_B, C_all = co_C,
                AandB = co_AB, AandC = co_AC, BandC = co_BC)

# alter some things, and save the outputs in a new variable  

Q <- vennChart(A_all = co_A, B_all = co_B, C_all = co_C,
                AandB = co_AB, AandC = co_AC, BandC = co_BC,
               colors = list(rgb(0.145, 0.51, 0.71, 0.4), 
                             rgb(1, 0.78, 0.16, 0.4),
                             rgb(1, 0.32, 0.16, 0.4)),
               textadj = c(1, 0.2),
               cex = 1.5,
               frameadj = 1.1
               )
      # change plot color
      # put the text of the three variables on the edge (\code{textadj[1] = 1}.)
      # larger text font
      # relatively smaller frame

# plot and print text from the output of \code{vennChart}

  plotVenn(Q$circles, frameadj = 1.2)
  plotVennTexts(Q$textspos, unit = "\%)", connect = ":\n(" )
  
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
