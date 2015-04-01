NOTES
======
The overall format appears to be:

    * all communication is via carrier pulses on 433.846 MHz
    * transmitter startup is signalled by a 26 ms pulse, followed by 5 sec delay. presumably during this time other units are to speak in allocated time-slots to assist the unit in selecting an unused ID
    * bits encoded as intervals between 200 µs pulses, interpreted as follows:
        * 1.2 ms (200 µs pulse, 1.0 ms delay) encodes 0
        * 2.2 ms (200 µs pulse, 2.0 ms delay) encodes 1
    
    * messages are repeated several times verbatim (typically 14), with 4.2 ms between each (again, measured start to start)
    
    * temp messages are 48 bits
    * interpret as follows (fields MSB first):
    
        aaaaaaaa BBBBbbbbbbbb CCCCcccccccc dddddddd xxxxxxxx
    
    * interpret sections as follows:
        
        * aaaaaaaa:         unknown (probe/session ID?)
        * BBBBbbbbbbbb:     "food" temp, in 0.1ºC
        * CCCCcccccccc:     "smoker" temp, in 0.1ºC
        * dddddddd:         flags, interpreted as:
            0x40: probe 1 present
            0x80: probe 2 present
            0x10: device powering up
            0x04: TX requested by user
            
        * xxxxxxxx:         checksum, computed as 2's complement of sum:
            aaaaaaaa + bbbbbbbb + BBBB + CCCC + cccccccc + dddddddd

    * temps are 12 bit signed 2's complement integers, displayed as follows (where x is value in message)
    
        * values out of calibration range are displayed as LLL/hhh
        * ºC displayed as floor(0.1  * x)
        * ºF displayed as floor(0.18 * x ) + 32

Other observed behaviors:

    * i'm not sure why but i've also occasionally observed a nine-bit message consisting of the temp probe ID followed by a zero bit
    * transmitter appears to freeze channel value when probe is yanked.  if unit starts with a missing probe, corresponding value is zero.  probe presence is indicated in 'flags' byte, as described above
    