source("stft.R")

# signal to error ratio (on amplitude spectrogram)

SER <- function(
	sig_x, sig_s,
	frame = 512,
	shift = 256,
	window = win.sine)
{
	spec_x <- stft(sig_x, frame, shift, win.sine)
	spec_s <- stft(sig_s, frame, shift, win.sine)
	X <- abs(spec_x)
	S <- abs(spec_s)

	SER <- sum(S**2)/sum((X - S)**2)
	SERdb <- 10 * log(SER) / log(10) 
	return (SERdb)
}


# test SER
# R --slave < ser.R
if( !interactive() ){
	sinewave <- function(freq) sin(1:160000 / 16000 * freq * 2 * pi)
	a <- sin(1:160000 / 16000 * 440 * 2 * pi)
	b <- sinewave(440) + sinewave(880)/2 + sinewave(1320)/3 + sinewave(1760)/4 + sinewave(3080) / 7
	c <- sinewave(440) + sinewave(880)/4 + sinewave(1320)/9 + sinewave(1760)/16 + sinewave(3080) / 49 + sinewave(4400)/100
	d <- sinewave(5460) + sinewave(5835)/4 + sinewave(5125)/9 + sinewave(2765)/16 + sinewave(7285) / 49 + sinewave(100)/100

	print(SER(a,b))
	print(SER(a,c))
	print(SER(a,a))
	print(SER(c,d))
}


