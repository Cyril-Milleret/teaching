\name{levin}
\alias{levin}
\title{Calculate levin's Index}
\usage{
levin(species,stand=TRUE)
}
\description{
Calculate levin Index from a vector. the vector should contain the percentage of food items consummed by a species.

}


\arguments{
\item{species}{Vector with the proportion of food items for the species }

\item{stand}{if TRUE then levins is standardized}


}

\examples{
names    <- c("rodents","insectivores","hare",
"carnivores","carrion","insects","plants",
"birds","ungulates")
red_fox <- c(29,9.8,15,2.5,29.4,1.3,9.5,3.5,0)
levin(red_fox,stand=TRUE)
}



