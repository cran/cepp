#Corrections
#Fixed Spherical distribution
#Fixed Volume Calculation
#Normalized Integral

pp.3 <- function(r=1,n,data,oth,given_norm=penorms3)
{
	require(randtoolbox)
	s	<- sobol(n,dim=3,scrambling=3)
	radius	<- s[,1] ^ (1/3)
	theta	<- 2 * asin(sqrt(s[,2]))
	psi	<- s[,3] * 2 * pi
	proj	<- matrix(0,ncol=3,nrow=ncol(data))
	srank.data	<- matrix(0,ncol=3,nrow=n)
	srank.oth	<- matrix(0,ncol=3,nrow=n)
	n1		<- nrow(data)
	n2		<- nrow(oth)
	index <- function(mat)
	{
		proj	<<- matrix(mat,ncol=3)
		proj.data<-  data %*% proj
		proj.oth<-   oth %*% proj
		al	<-  t(rbind(proj.data,proj.oth))
		ev	<-  evaluator.3(t(al))
		spmed	<-  trust(ev,parinit=c(median(al[1,]),median(al[2,]),median(al[3,])),u=c(0,0,0),rinit=0.5,rmax=2e5)
		tmp	<-  al - spmed$argument
		tmax	<-  sqrt(max(penorm3(tmp[1,],tmp[2,],tmp[3,])))
		Qr	<-  tmax * r * radius
		#To Cartesian
		X	<- Qr * sin(theta) * cos(psi)
		Y	<- Qr * sin(theta) * sin(psi)
		Z	<- Qr * cos(theta)
		o.Q	<- cbind(X,Y,Z)
		#End
		Q	<-  t(o.Q) - spmed$argument
		proj.data	<- t(proj.data)
		proj.oth	<- t(proj.oth)
		for(i in 1:n)
		{
			one		<- proj.data - Q[,i]
			dnorms		<- sqrt(penorm3(one[1,],one[2,],one[3,]))
			srank.data[i,]	<- colSums(t(one) / dnorms)
			two		<- proj.oth - Q[,i]
			onorms		<- sqrt(penorm3(two[1,],two[2,],two[3,]))
			srank.oth[i,]	<- colSums(t(two) / onorms)
		}
		tmp <- srank.data - srank.oth
		return(mean(given_norm(tmp[,1],tmp[,2],tmp[,3])))
	}
	return(index)
}
