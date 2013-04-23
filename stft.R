
# STFT for R
#  Hideyuki Tachibana 2012, 2013

# fft
#------------------------------------------------------------------------------
fft.half <- function(x) fft(x)[1:(length(x)/2+1)]
ifft.half <- function(x) Re(fft(c(x,Conj(rev(x[2:(length(x)-1)]))),inverse=T))/(2 * length(x) - 2)

# basic window functions
#------------------------------------------------------------------------------
win.rect <- function(x) 1:length(x) * 0 + 1
win.sine <- function(x) sin(x/length(x) * pi)
win.hann <- function(x) 0.5 - 0.5 * cos(x / length(x) * 2 * pi)
win.hamming <- function(x) 0.54 - 0.46 * cos(x / length(x) * 2 * pi)
win.blackman <- function(x) 0.42 - 0.5 * cos(x / length(x) * 2 * pi) + 0.08 * cos(x / length(x) * 4 * pi)

# stft
#------------------------------------------------------------------------------
stft <- function(signal, 
	frame = 512, 
	shift = 256, 
	window = win.sine)
{
	n.frame <- (length(signal) + frame / 2) %/% shift + 1
	sig.ext <- c(1:(frame/2) * 0, signal, 1:(frame*2) * 0)
	
	return(
		do.call("rbind",
			lapply(
				0:(n.frame - 1), # for all frames
				function(x)( fft.half( # apply fft
					window(1:frame) * sig.ext[1:frame + shift * x])))))
}

# normalization factor of the window function for inverse transform
# (win.forward(x) * win.inverse(x)) `conv` sum_n delta(x - shift * n)
#------------------------------------------------------------------------------
inverse.window.normalization.factor <- function(frame, shift, win.forward, win.inverse)
	colSums(do.call("rbind",
		lapply(
			- (frame %/% shift + 1) : (frame %/% shift + 1),
			function(x)( c(
				1:(frame * 2) * 0,
				win.forward(1:frame) * win.inverse(1:frame),
				1:(frame * 2) * 0)
					[(frame * 2) + shift * x + 1:frame]))))

# istft
#------------------------------------------------------------------------------
istft <- function(spectrogram, 
	shift = 256, 
	win.forward = win.sine, 
	win.inverse = win.sine, 
	total.length)
{
	frame <- length(spectrogram[1,]) * 2 - 2
	n.frame <- length(spectrogram[,1])
	win.modify <- inverse.window.normalization.factor(frame, shift, win.forward, win.inverse)
	
	sigs <- do.call("rbind",
		lapply(
			1:n.frame,
			function(x) (ifft.half(spectrogram[x,]) * win.inverse(1:frame) / win.modify[1:frame])))

	sig <- 1:(total.length + frame * 3) * 0
	for(x in 1:(n.frame - 1)){
		sig[shift * (x - 1) + 1:frame] <- sig[shift * (x - 1) + 1:frame] + sigs[x,1:frame]
	}
	return(sig[frame/2 + 1:total.length]) 
}

# sample signal
#------------------------------------------------------------------------------
sinewave <- function(freq) sin(1:80000 / 16000 * freq * 2 * pi)
sample.signal.a <- sin(1:160000 / 16000 * 440 * 2 * pi)
sample.signal.b <- sinewave(440) + sinewave(880)/2 + sinewave(1320)/3 + sinewave(1760)/4 + sinewave(3080) / 7
sample.signal.c <- sinewave(440) + sinewave(880)/4 + sinewave(1320)/9 + sinewave(1760)/16 + sinewave(3080) / 49 + sinewave(4400)/100



