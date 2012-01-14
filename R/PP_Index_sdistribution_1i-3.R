#Corrections
#Correctly generate from uniform distribution over the circle
#Correctly evaluate integral by multipying with right area

pp.2 <- function(r=1,n,data,oth,given_norm=penorms2)
{
	require(randtoolbox)
	s	<- sobol(n,dim=2,scrambling=3)
	radius	<- sqrt(s[,1])
	angle	<- s[,2] * 2 * pi
	proj	<- matrix(0,ncol=2,nrow=ncol(data))
	srank.data	<- matrix(0,ncol=2,nrow=n)
	srank.oth	<- matrix(0,ncol=2,nrow=n)
	n1		<- nrow(data)
	n2		<- nrow(oth)
	index <- function(mat)
	{
		proj	<<- matrix(mat,ncol=2)
		proj.data<-  data %*% proj
		proj.oth<-   oth %*% proj
		al	<-  t(rbind(proj.data,proj.oth))
		ev	<-  evaluator.2(t(al))
		spmed	<-  trust(ev,parinit=c(median(al[1,]),median(al[2,])),u=c(0,0),rinit=0.5,rmax=2e5)
		tmp	<-  al - spmed$argument
		tmax	<-  sqrt(max(penorm2(tmp[1,],tmp[2,])))
		Qr	<-  tmax * r * radius
		#To Cartesian
		X	<-  Qr * cos(angle)
		Y	<-  Qr * sin(angle)
		o.Q	<-  cbind(X,Y)
		#End
		Q	<-  t(o.Q) - spmed$argument
		proj.data	<- t(proj.data)
		proj.oth	<- t(proj.oth)
		for(i in 1:n)
		{
			one		<- proj.data - Q[,i]
			dnorms		<- sqrt(penorm2(one[1,],one[2,]))
			srank.data[i,]	<- colSums(t(one) / dnorms)
			two		<- proj.oth - Q[,i]
			onorms		<- sqrt(penorm2(two[1,],two[2,]))
			srank.oth[i,]	<- colSums(t(two) / onorms)
		}
		tmp <- srank.data - srank.oth
		return(mean(given_norm(tmp[,1],tmp[,2])))
	}
	return(index)
}
