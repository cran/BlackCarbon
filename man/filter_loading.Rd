\name{filter_loading}
\alias{filter_loading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Filter loading correction
%%  ~~function to do ... ~~
}
\description{

This function is applied when we already have black carbon concentration processed by ONA algorithm. Since Black carbon concentration measured by AE51 substantially decreases when the filter loading goes up, we can apply filter correction function to adjust for that.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter_loading(data, ONA_BC = "BC_ONA", ATN = "ATN")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  a datafram containing the raw data of black carbon.
%%     ~~Describe \code{data} here~~
}
  \item{ONA_BC}{
  the column having black carbon concentration processed by ONA algorithm.
%%     ~~Describe \code{ONA_BC} here~~
}
  \item{ATN}{
  the column having ATN coefficient.
%%     ~~Describe \code{ATN} here~~
}
}
\details{
This method is applicable even when the raw files are compiled as it uses the corresponding ATN values to correct for filter loading of black carbon concentration
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The default mthod returns a vector with equal length as the dataframe.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kirchstetter, T.W., Novakov, T., 2007. Controlled generation of black carbon particles from a diffusion flame and applications in evaluating black carbon measurement methods. Atmospheric Environment 41, 1874-1888 Available from: https://doi.org/10.1016/j.atmosenv.2006.10.067
%% ~put references to the literature/web site here ~
}
\author{
Sanjeev Bista (MPH - Advanced Bio-statistics and Epidemiology)
%%  ~~who you are~~
}
\note{
Higly recommend to read the reference before using the function
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ONA}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(data_ONA)#loading the example dataset
#calculating ONA processed black carbon first
data_ONA$ONA_BC<-ONA(data=data_ONA, ATN = "ATN", BC ="BC", dATN =0.05)
filter_loading(data =data_ONA, ATN = "ATN", ONA_BC="BC")

## The function is currently defined as
function (data, ONA_BC = "BC_ONA", ATN = "ATN")
{
    data[, ONA_BC] * (0.88 * exp(-(data[, ATN])/100) + 0.12)^(-1)
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
