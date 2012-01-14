\name{Olive oil measurements}
\docType{data}
\alias{olive}
\title{Olive oil samples from Italy}
\description{
This data is from a paper by Forina, Armanino, Lanteri, Tiscornia (1983) Classification of Olive Oils from their Fatty Acid Composition, in Martens and Russwurm (ed) Food Research and Data Anlysis. We thank Prof. Michele Forina, University of Genova, Italy for making this dataset available.

\itemize{
\item region Three super-classes of Italy: North, South and the island of Sardinia
\item area Nine collection areas: three from North, four from South and 2 from Sardinia
\item palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, arachidic, eicosenoic fatty acids percent x 100
}
}

\usage{data(olive)}
\format{A 572 x 10 numeric array}
\keyword{datasets}
\examples{
data(olive)
head(olive)
##Permutation
OlivesT <- as.matrix(olive[,-c(1:2)])
OlivesF <- OlivesT
#You should set.seed here so as to "fix" the benchmark
OlivesF[,'palmitic']	<- OlivesF[sample(572,572),'palmitic']
OlivesF[,'palmitoleic'] <- OlivesF[sample(572,572),'palmitoleic']
OlivesF[,'stearic'] 	<- OlivesF[sample(572,572),'stearic']
OlivesF[,'oleic'] 	<- OlivesF[sample(572,572),'oleic']
OlivesF[,'linoleic'] 	<- OlivesF[sample(572,572),'linoleic']
OlivesF[,'linolenic']	<- OlivesF[sample(572,572),'linolenic']
OlivesF[,'arachidic']	<- OlivesF[sample(572,572),'arachidic']
OlivesF[,'eicosenoic']	<- OlivesF[sample(572,572),'eicosenoic']
##
oil1 <- pp.2(r=2,n=50,oth=OlivesF,data=OlivesT)
##In practice try at least >10 starting values
F1   <- basis_random(8)
##Increase iterations to >2000 for useful results
o1   <- optim(par=F1, fn=oil1,gr=basis_nearby,method='SANN',control=list(fnscale=-1,maxit=50,trace=6))
}
