#mv
mv <- function(input, output){
	cmdline <- paste(
	  "mv", input, output,
	  sep=" ")
	system(cmdline)
}

#cp
cp <- function(input, output){
	cmdline <- paste(
	  "cp", input, output,
	  sep=" ")
	system(cmdline)
}

#sox example
sox <- function(input, output){ 
	cmdline <- paste(
	  "sox ", input, " -r 8000 -c 1 ", output, " trim 0 20",
	  sep="")
	system(cmdline)
}

#convert
convert <- function(input, output){
	cmdline <- paste(
	  "convert", input, output,
	  sep=" ")
	system(cmdline)
}
