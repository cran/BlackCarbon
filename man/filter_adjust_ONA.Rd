\name{filter_adjust_ONA}
\alias{filter_adjust_ONA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ONA adjusting filter change
%%  ~~function to do ... ~~
}
\description{
This function is an extended version of (ONA and filter_loading) which can only work at individual file level, similar to the ONA function. This function has an inbuilt function of filter loading correction as well as has capacity to adjust for filter change or a huge drop in ATN marked as mechanical error.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter_adjust_ONA(data =data, ATN = "ATN", BC = "BC", dATN = 0.05,
threshold = 5, skip = 15, ignore = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a datafram containing the raw data of black carbon.

%%     ~~Describe \code{data} here~~
}
  \item{ATN}{
the coloumn having ATN coefficient.
%%     ~~Describe \code{ATN} here~~
}
  \item{BC}{
  the column having raw black carbon data.
%%     ~~Describe \code{BC} here~~
}
  \item{dATN}{
  the difference in ATN coefficient which is used for defining the averaging window of raw black carbon concentration, by default it is 0.05.
%%     ~~Describe \code{dATN} here~~
}
  \item{threshold}{
  the change in ATN value which indicates the filter change, it is 5 by default.
%%     ~~Describe \code{threshold} here~~
}
  \item{skip}{
number of observations before the filter change point that has to be skipped or deleted from analysis. When the AE51 device is about to indicate the need of filter change or when it’s about encounters a sudden drop of ATN it either produces maximum negative or positive black carbon concentration which is a mechanical noise and averaging those values also introduces bias. Therefore certain observations before filter change or extreme drop down of ATN should be deleted depending on the data collection interval. By default it is 15 observation for 10s interval data.
%%     ~~Describe \code{skip} here~~
}
  \item{ignore}{
number of observations after the filter change point that has to be ignored or deleted from analysis. After changing the filter or after a while of sudden drop of ATN value, AE51 device either produces maximum negative or positive black carbon concentration which is a mechanical noise and averaging those values also introduces bias. Therefore certain observations after filter change or sudden drop of ATN should be deleted depending on the data collection interval. By default it is 10 observation for 10s interval data.
%%     ~~Describe \code{ignore} here~~
}
}
\details{
This method is only applicable when the raw files are not compiled
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The default method returns the existing data frame with two additional columns 1) with ONA processed black carbon concentration and 2) filter loading corrected ONA processed black carbon concentration. If there is a filter change or a drop of ATN by more than 5 units within two adjoining observations some observations (25 rows), which is ignore + skip given in the function,  will be deleted to correct for the mechanical error

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Hagler GSW, Yelverton TLB, Vedantham R, Hansen ADA, Turner JR. Post-processing Method to Reduce Noise while Preserving High Time Resolution in Aethalometer Real-time Black Carbon Data. Aerosol Air Qual Res [Internet]. 2011 Oct [cited 2020 May 3];11(5):539–46. Available from: http://www.aaqr.org/doi/10.4209/aaqr.2011.05.0055

Kirchstetter, T.W., Novakov, T., 2007. Controlled generation of black carbon particles from a diffusion flame and applications in evaluating black carbon measurement methods. Atmospheric Environment 41, 1874-1888 Available from: https://doi.org/10.1016/j.atmosenv.2006.10.067


%% ~put references to the literature/web site here ~
}
\author{
Sanjeev Bista (MPH - Advanced Bio-statistics and Epidemiology)
%%  ~~who you are~~
}
\note{
When there is a filter change or a rapid drop of ATN value, as opposed to the assumption that the ATN should gradually increase, the averaging window identified using dATN becomes wider or gets violated as it doesn't/may not get a proper window till the last observation. Therefore, an averaging window should be then identified from the very next point to the rapid drop of ATN, considering it as if an initial point of a new file.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ONA}}
\code{\link{filter_loading}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(data_ONA)#loading the example dataset
#saving a processed dataset with the same name
data_ONA<-filter_adjust_ONA(data =data_ONA, ATN = "ATN", BC = "BC", dATN = 0.05, threshold = 5,
                  skip = 15, ignore = 10)


## The function is currently defined as
function (data =data, ATN = "ATN", BC = "BC", dATN = 0.05, threshold = 5,
    skip = 15, ignore = 10)
{
    if (nrow(data) < (skip + ignore)) {
        print("not enough observations to correct for filter loading")
    }
    else {
        for (j in skip:(nrow(data) - ignore)) {
            if ((data[, ATN][j] - data[, ATN][j - 1]) < (-threshold)) {
                f <- j
                k1 <- data[1:(f - skip), ]
                k2 <- data[(f + ignore):nrow(data), ]
                k1[, "BC_ONA"] <- ONA(data = k1, ATN = "ATN",
                  BC = "BC", dATN = dATN)
                k1[, "BC_ONA_Cor"] <- k1[, "BC_ONA"] * (0.88 *
                  exp(-(k1[, ATN])/100) + 0.12)^(-1)
                k2[, "BC_ONA"] <- ONA(data = k2, ATN = "ATN",
                  BC = "BC", dATN = dATN)
                k2[, "BC_ONA_Cor"] <- k2[, "BC_ONA"] * (0.88 *
                  exp(-(k2[, ATN])/100) + 0.12)^(-1)
            }
        }
    }
    if (exists("k1") == T) {
        data <- rbind(k1, k2)
        rm(k1, k2)
    }
    else {
        data[, "BC_ONA"] <- ONA(data, ATN = "ATN", BC = "BC",
            dATN = 0.1)
        data[, "BC_ONA_Cor"] <- data[, "BC_ONA"] * (0.88 * exp(-(data[,
            ATN])/100) + 0.12)^(-1)
    }
    return(data)
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
