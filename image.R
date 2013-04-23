
# plotting functions (under construction)
#  Hideyuki Tachibana 2012, 2013

# tanh
tanh.normalized <- function(xs) {
	sd. <- sd(c(xs))
	tanh((xs - median(xs) - 3 * sd.)/(2*sd.))
}

# color
rainbow.color.set <- 
	list(
		func=function(z)(-tanh.normalized(abs(z)**0.1)), 
		col=rainbow(256, start = 0, end = 0.7))

gray.color.set <- 
	list(
		func = function(z)(- tanh.normalized(abs(z)**0.1)), 
		col = gray.colors(1000, start = 0, end = 1))

# image plot function
image.print <- function(matrix, color.set){
	image(color.set$func(matrix), col = color.set$col)
}

