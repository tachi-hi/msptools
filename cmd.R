#mv
mv(input, output){
	cmdline <- paste(
	  "mv", input, output,
	  sep=" ")
	system(cmdline)
}

#cp
cp(input, output){
	cmdline <- paste(
	  "cp", input, output,
	  sep=" ")
	system(cmdline)
}

#sox example
sox(input, output){ 
	cmdline <- paste(
	  "sox ", input, " -r 8000 -c 1 ", output, " trim 0 20",
	  sep="")
	system(cmdline)
}

#convert
png2eps(input, output){
	cmdline <- paste(
	  "convert", input, output,
	  sep=" ")
	system(cmdline)
}
