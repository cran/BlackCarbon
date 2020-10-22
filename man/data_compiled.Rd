\name{data_compiled}
\alias{data_compiled}
\docType{data}
\title{Dataset compiled at file level
%%   ~~ data name/kind ... ~~
}
\description{
This is a hypothetical dataset (compiled at file level) created as an example to faciliate user's learning of the function used in the package while processing the black carbon data with ONA algorithm
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("data_compiled")}
\format{
  A data frame with 31118 observations on the following 3 variables.
  \describe{
    \item{\code{file_ID}}{a factor with levels \code{AE51-S6-1211_20181114-025500} \code{AE51-S6-1211_20181115-070300} \code{AE51-S6-1211_20181115-201000}}
    \item{\code{ATN}}{a numeric vector}
    \item{\code{BC}}{a numeric vector}
  }
}

\examples{
data(data_compiled)
## maybe str(data_compiled)
}
\keyword{datasets}
