

# mode
mode.of.distribution <- function(dist){
	hst <- hist(dist, plot = FALSE)
	return(hst$breaks[which.max(hst$counts)])
}

# boxcox
boxcox <- function(xs, e) (xs ** e - 1)/e

# composition of functions
cf <- function(f,g) function(x) (f(g(x)))


