\name{ONA_compiled}
\alias{ONA_compiled}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ONA at file level
%%  ~~function to do ... ~~
}
\description{
This function is an extended version of filter_adjust_ONA which can work with the files compiled at person level or at study level, unlike the filter_adjust_ONA function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ONA_compiled(data = data, ATN = "ATN", BC = "BC", file_id = "file_ID", dATN = 0.05,
threshold = 5, skip = 15, ignore = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  a datafram containing the raw data of black carbon.
%%     ~~Describe \code{data} here~~
}
  \item{ATN}{
%%     ~~Describe \code{ATN} here~~
}
  \item{BC}{
  the column having ATN coefficient.
%%     ~~Describe \code{BC} here~~
}
  \item{file_id}{
  the column having file id after compiling files produced at different time for a person or of different participants.
%%     ~~Describe \code{file_id} here~~
}
  \item{dATN}{
  the difference in ATN coefficient which is used for defining the averaging window of raw black carbon concentration, by default it is 0.05.
%%     ~~Describe \code{dATN} here~~
}
  \item{threshold}{
  the change in ATN value which indicates the filter change or the rapid drop of ATN value that may violate the averaging window which is based on the assumption that the ATN gradually increases.By default the value of 5 is used.
%%     ~~Describe \code{threshold} here~~
}
  \item{skip}{
number of observations before the filter change point that has to be skiped or deleted from analysis. When the AE51 device is about to indicate the need of filter change it either produces maximum negative or positive black carbon concentration which is a mechanical noise and averaging those values also introduces bias. Therefore certain observations before filter change should be deleted depending on the data collection interval. By default it is 15 observation for 10s interval data.
%%     ~~Describe \code{skip} here~~
}
  \item{ignore}{
number of observations after the filter change point that has to be ignored or deleted from analysis. After chaning the filter AE51 device either produces maximum negative or positive black carbon concentration which is a mechanical noise and averaging those values also introduces bias. Therefore certain observations after filter change should be deleted depending on the data collection interval. By default it is 10 observation for 10s interval data.
%%     ~~Describe \code{ignore} here~~
}
}
\details{
This method also applicable when the raw files are compiled at person level or even at study level
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The default method returns the existing data frame with two additional columns 1) with ONA processed black carbon concentration and 2) filter loading corrected ONA processed black carbon concentration.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Hagler GSW, Yelverton TLB, Vedantham R, Hansen ADA, Turner JR. Post-processing Method to Reduce Noise while Preserving High Time Resolution in Aethalometer Real-time Black Carbon Data. Aerosol Air Qual Res [Internet]. 2011 Oct [cited 2020 May 3];11(5):539–46. Available from: http://www.aaqr.org/doi/10.4209/aaqr.2011.05.0055

Kirchstetter, T.W., Novakov, T., 2007. Controlled generation of black carbon particles from a
diffusion flame and applications in evaluating black carbon measurement methods.
Atmospheric Environment 41, 1874-1888
%% ~put references to the literature/web site here ~
}
\author{
Sanjeev Bista (MPH - Advanced Bio-statistics and Epidemiology)
%%  ~~who you are~~
}
\note{
The file id helps the function to distinguish each separate file created by AE51 device at different point in time or created for different participants and assists in processing the file individually and compile them back to make a single file as it was given to the function.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

\code{\link{ONA}}
\code{\link{filter_loading}}
\code{\link{filter_adjust_ONA}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}


\examples{

data(data_compiled) #loading the example dataset compiled at file level
#saving a processed dataset (compiled at file level) with the same name
data_compiled<-ONA_compiled(data = data_compiled, ATN = "ATN", BC = "BC", file_id = "file_ID",
          dATN = 0.05, threshold = 5, skip = 15, ignore = 10)

## The function is currently defined as
function (data = data, ATN = "ATN", BC = "BC", file_id = "file_ID",
    dATN = 0.05, threshold = 5, skip = 15, ignore = 10)
{
    data[, "file_ID"] <- as.factor(data[, "file_ID"])
    data1 <- lapply(split(data, data[, file_id]), function(x) {
        filter_adjust_ONA(x, ATN = ATN, BC = BC, dATN = dATN,
            threshold = threshold, skip = skip, ignore = ignore)
    })
    names(data1) <- NULL
    data2 <- do.call("rbind", data1)
    return(data2)
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
