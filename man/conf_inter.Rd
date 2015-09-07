\name{conf_inter}
\alias{conf_inter}
\title{Add confidence intervals to a plot}
\usage{
conf_inter(x, y, se=SE, stder=TRUE, low_ci=lowci, up_ci=upci, z=0.1,col="black",lty=1, lwd=1)}
\description{
Add confidence intervals to a plot. Provide x and y vectors. Values can be Standard error (stder= TRUE,SE=SE) or upper condidence intervals and lower confidence itnervals (stder= FALSE).

}


\arguments{
\item{x}{vectors of values on the x axis}

\item{y}{vectors of values on the y axis}

\item{SE}{vectors of values on the for the standard error if stder=TRUE}

\item{stder}{TRUE if you have SE values (1.96*SE), FALSE if you already have CI limits}

\item{low_ci}{vector with lower confidence limits}
\item{up_ci}{vector with upper confidence limits}

\item{z}{how large should the end of the forizontal line of the confidence interval, default is 0.1}
\item{col}{color, default is black}
\item{lwd}{lwd line width, default is 1}
\item{lty}{ line type, default is 1}


}

\examples{
names    <- c("rodents","insectivores","hare",
"carnivores","carrion","insects","plants",
"birds","ungulates")
red_fox <- c(29,9.8,15,2.5,29.4,1.3,9.5,3.5,0)
levin(red_fox,stand=TRUE)
}
