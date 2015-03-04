from pylab import *
from scipy.signal import *
import matplotlib.pyplot as plt
import numpy as np

sample_rate = 250e3
nyq_rate    = 0.5 * sample_rate
decimation  = 4
cutoff      = 0.4 / decimation
transition  = 0.1 / decimation

pass_db     = 3.0
stop_db     = 60.0


# N, beta     = kaiserord(stop_db, 2 * transition)
# taps        = firwin(N, 2 * cutoff, window=('kaiser', beta))

# N = 128
# dc_notch = 1./N
# taps = remez(N, [0, dc_notch, 2*dc_notch, cutoff, cutoff + transition, 0.5], [0, 1,0])

# N = 32
# taps = remez(N, [0, cutoff, cutoff + transition, 0.5], [1,0])

# b = taps
# a = array([1.0])

# b, a = iirdesign(cutoff, cutoff + transition, pass_db, stop_db)

b, a = iirfilter(1, [0.01, 0.1], # pass_db, stop_db,
                        btype='band', ftype='bessel')

#------------------------------------------------
# Plot the magnitude response of the filter.
#------------------------------------------------

figure(1)
clf()
w, h = freqz(b, a, worN=8000)

# plot((w/pi)*nyq_rate, 10*log(absolute(h)), linewidth=2)
# xlabel('Frequency (Hz)')
# ylabel('Gain (dB)')
# title('Frequency Response')
# ylim(-50, 6)
# grid(True)

fig = plt.figure()
plt.title('Digital filter frequency response')
ax1 = fig.add_subplot(111)

plt.plot(w, 20 * np.log10(abs(h)), 'b')
plt.ylabel('Amplitude [dB]', color='b')
plt.xlabel('Frequency [rad/sample]')

ax2 = ax1.twinx()
angles = np.unwrap(np.angle(h))
plt.plot(w, angles, 'g')
plt.ylabel('Angle (radians)', color='g')
plt.grid()
plt.axis('tight')
plt.show()

print(b)
print(b.size)
print(a)
print(a.size)
