import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import time
import serial
import struct
import kconvert
xsize=150

ser = serial.Serial(
 port='COM3',
 baudrate=115200,
 parity=serial.PARITY_NONE,
 stopbits=serial.STOPBITS_ONE,
 bytesize=serial.EIGHTBITS
)
ser.isOpen()  
def data_gen():
    t = data_gen.t
    while True:
       
       strin = ser.readline()
       t+=1
       val=float(strin)
     #  val/=1000
       val*=15000
       val/=470000
       val*=4.11
       cj=0
       val/=1024
       val*=1000
       val=round(kconvert.mV_to_C(val,cj),1)
       val =int(val)
 
       temp = str(val).zfill(4)+'\n'

       ser.write(temp.encode('ascii'))
       yield t, val


       

def run(data):
    # update the data
    t,y = data
    
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)

    return line,

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)
ax.set_ylim(0,250 )
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []


# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()
