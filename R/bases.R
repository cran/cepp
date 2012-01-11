basis_random <- function (n, d = 2) 
{
    mvn <- matrix(rnorm(n * d), ncol = d)
    orthonormalise(mvn)
}

basis_nearby <- function (current, alpha = ALPHA, method = METHOD) 
{
    current <- matrix(current, ncol = 2)
    method <- match.arg(method, c("linear", "geodesic"))
    new <- basis_random(nrow(current), ncol(current))
    switch(method,
		linear = as.numeric(orthonormalise((1 - alpha) * current + alpha * new)), 
		geodesic = as.numeric(step_fraction(geodesic_info(current, new), alpha)))
}
