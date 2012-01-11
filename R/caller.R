caller <- function(start,index,n,bases)
{
	p <- vector('list',bases+1)
	i <- 1
	cat("Initial Value",index(start),'\n')
	p[[1]] <- start
	while (i <= bases)
	{
		p[[i+1]]<- search_geodesic(current=p[[i]],index=index,n=n[i],max.tries=10)
		if((length(p[[i+1]]) == 1)) return(p[1:i])
		i	<- i + 1
	}
	return(p)
}
