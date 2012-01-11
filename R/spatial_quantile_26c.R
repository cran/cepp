#Works in 3-D
evaluator.3 <- function(samp)
{
	samp <- samp
	n <- nrow(samp)
	p <- ncol(samp)

	func <- function(at,u)
	{
		phi <- function(u,t)	sqrt(sum(t^2)) + sum(u*t)

		diff <- t(t(samp) - at)
		scales <- sqrt(penorm3(diff[,1],diff[,2],diff[,3]))
		scaled.diff <- diff / scales
		phis <- apply(diff,MARGIN=1,phi,u=u)
		result <- sum(phis)
		
		delta <- colSums(scaled.diff) + n*u

		inner <- apply(scaled.diff,MARGIN=1,FUN=tcrossprod) #returns a column of values for each row of input
		acc <- rep(0,p)
		for(i in 1:ncol(inner))
		{	
			m <- matrix(inner[,i],p)
			acc <- acc + (diag(p) - m) / scales[i]
		}

		return(list(value=result,gradient=-delta,hessian=acc))
	}
}
