\name{ONA}
\alias{ONA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Optimized Noise Reduction Averaging (ONA) algorithm
  %%  ~~function to do ... ~~
}
\description{
  The function uses the Optimized Noise Reduction Averaging (ONA) algorithm to process the raw black carbon data collected by Aethalometers (MicroAeth AE51, AethLabs, CA, USA).The algorithm smooths the black carbon concentration over a varying smoothing window identified by change in coefficient of light attenuation (ATN).
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  ONA(data=data, ATN = "ATN", BC ="BC", dATN =0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ it is a datafram generated by the AE51 device at the file level and not compiled with other files, however, a little pre-processing has to be done such as naming the columns and formatting it as a data frame.
    %%     ~~Describe \code{x} here~~
  }
  \item{ATN}{
the column having ATN coefficient.
    %%     ~~Describe \code{x} here~~
  }
  \item{BC}{
the column having raw black carbon data.
    %%     ~~Describe \code{x} here~~
  }
  \item{dATN}{the difference in ATN coefficient which is used for defining the averaging window of raw black carbon concentration, by default it is 0.05.
    %%     ~~Describe \code{x} here~~
  }
}
\details{

  This method is only applicable when the raw files are not compiled
  %%  ~~ If necessary, more details than the description above ~~
}
\value{

  The default method returns a vector with equal length as the dataframe
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{

  Hagler GSW, Yelverton TLB, Vedantham R, Hansen ADA, Turner JR. Post-processing Method to Reduce Noise while Preserving High Time Resolution in Aethalometer Real-time Black Carbon Data. Aerosol Air Qual Res [Internet]. 2011 Oct [cited 2020 May 3];11(5):539–46. Available from: http://www.aaqr.org/doi/10.4209/aaqr.2011.05.0055
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
  \code{\link{mean}}
  \code{\link{lapply}}
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }

  \examples{
  data(data_ONA) # loading example dataset
  ONA(data=data_ONA, ATN = "ATN", BC ="BC", dATN =0.05)

  data(data_compiled) #loading the example dataset compiled at file level
  #applying ONA on a compiled file where data come from different files
  data_compiled$file_ID<-as.factor(data_compiled$file_ID)
  data_compiled$ONA_BC<-unlist(lapply(split(data_compiled, data_compiled$file_ID),
  function(x) ONA(data=x, ATN = "ATN", BC ="BC", dATN =0.05)))

  }
