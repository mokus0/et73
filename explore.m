% annotated excerpts from raw-data exploration session in octave

% raw IQ data (captured at 250 ksps, low-pass filtered and decimated to 15.625 ksps)
t = dlmread('temp.txt');
z = t(:,1) + t(:,2)*i;

% zooming in and looking at one of the messages
m = z(3.7e4:6.2e4);
p = 10*log10(abs(m));
b = p>15;
b2 = b(2:end) > b(1:end-1);
ts = find(b2);
dts = diff(ts);

% in this case, the selected region was one message repeated 14 times
msgs = reshape([dts(:); 65], 49,14)';

% now zooming back to whole series
p = 10*log10(abs(z));
b = p>15;
b2 = b(2:end) > b(1:end-1);
ts = find(b2);
dts = diff(ts);
msgs = reshape([dts(1:3430)], 49,70)';
msgs_trimmed = msgs(:,1:48); % stripping intervals between messages
