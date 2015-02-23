from pylab import *
from scipy.signal import *

sample_rate = 250e3
nyq_rate    = 0.5 * sample_rate

# cutoff      = 0.08 * sample_rate
# transition  = 0.02 * sample_rate
# stop_db     = 40.0

# cutoff      = 0.2  * sample_rate
# transition  = 0.05 * sample_rate
# stop_db     = 40.0

# N, beta     = kaiserord(stop_db, transition/nyq_rate)
# taps        = firwin(N, cutoff/nyq_rate, window=('kaiser', beta))

N = 128
taps = remez(N, [0, 0.00078125, 0.005, 0.03, 0.03125, 0.5], [0, 1,0])

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
