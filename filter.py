from pylab import *
from scipy.signal import *

sample_rate = 250e3
nyq_rate    = 0.5 * sample_rate
decimation  = 10
cutoff      = 0.4 / decimation
transition  = 0.1 / decimation

stop_db     = 50.0
N, beta     = kaiserord(stop_db, 2 * transition)
taps        = firwin(N, 2 * cutoff, window=('kaiser', beta))

# N = 128
# dc_notch = 1./N
# taps = remez(N, [0, dc_notch, 2*dc_notch, cutoff, cutoff + transition, 0.5], [0, 1,0])

# N = 256
# taps = remez(N, [0, cutoff, cutoff + transition, 0.5], [1,0])

#------------------------------------------------
# Plot the magnitude response of the filter.
#------------------------------------------------

figure(1)
clf()
w, h = freqz(taps, worN=8000)
plot((w/pi)*nyq_rate, 10*log(absolute(h)), linewidth=2)
xlabel('Frequency (Hz)')
ylabel('Gain (dB)')
title('Frequency Response')
# ylim(-0.05, 1.05)
grid(True)


print(taps)
print(taps.size)
show()
