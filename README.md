University of British Columbia  
ELEC 291

Team B8  
- Bryan Nsoh
- Will Chaba
- Isabelle Andre
- Debby Lin
- Eric Wu
- Daniel Nadeem

# Project 1 Reflow Oven Controller

A reflow oven controller connected to an oven to perform reflow soldering of electronic components onto a printed circuit board (PCB).

## Project Details

This project consisted in designing and programming a reflow oven controller that would heat up and attach components to the solder pads of an EFM8 PCB.
Reflow soldering is a common soldering technique used to assemble surface mount devices onto the PCB's pads coated with solder paste.
A heating process with mutliple stages of different temperature range melts the paste, thus linking the components. The designed reflow oven controller sets and executes the settings of a complete reflow process consisting of 6 states numbers 0 to 5 using a finite state machine. Folowing these states, the oven had to maintain set soak and reflow temperatures for specific time periods for each cycle of the reflow process. The controller varies the amount of power delivered to the oven via an SSR box through pulse width modulation (PWM). Temperature is read by inserting one end of a k-type thermocouple with cold junction compensation into the oven during the reflow process. The temperature is annouced every 5 seconds by speaker voice feedback as well as any state changes. As a safety feature, if the temperature read by the thermocouple did not rise to at least 60 degrees within the first 60 seconds of operation upon starting the process, the controller terminates the process.

<img src="images/board.jpg" width="700"/>

*Completed Reflow Over Controller Circuit Board*

## Controller State Machine

The finite state machine implemented consists of 6 states.

<img src="images/reflow_states.PNG" width="500"/>

*Reflow States*

### Reflow States

#### State 0: Settings

State 0 allows the soak and reflow temperature to be set before starting the process. A user interface with an liquid crystal display (LCD) screen and push buttons is used to choose soak temperature and time and reflow temperature and time. The user presses the Start button to proceed to the next state.

#### State 1: Ramp to Soak

In State 1, time is allocated to heat the oven until soak temperature is reached. The oven heats up at maximum power, usually heating up by 1-3 degrees per second. Current temperature is continually compared to the set soak temperature.  Voice feedback dictates the current temperature every 5 seconds. When the temperature has reached soak temperature, the code proceeds to State 2.

#### State 2: Soak

The temperature is kept stable at chosen temperature and the soak timer is enabled. Once the timer ends, State 3 begins.

#### State 3: Ramp to Reflow

Similarily to State 1, time is allocated to heat the oven until reflow temperature is reached. The oven heats up at maximum power, usually heating up by 1-3 degrees per second. Current temperature is continually compared to the set reflow temperature.  Voice feedback dictates the current temperature every 5 seconds. When the temperature has reached reflow temperature, the code proceeds to State 4.

#### State 4: Reflow

The temperature is kept stable at chosen temperature and the reflow timer is enabled. Once the timer ends, State 5 begins.

#### State 5: Cooling

At State 5, the oven is allowed to cool until the temperature reaches 60 degrees, after which the process ends and resets to State 1.

The approximate reflow settings used in the final demo can be observed in this table.

<img src="images/reflow_settings" width="500"/>

*Reflow Settings Temperature and Time*

### Software Block Diagram

<img src="images/software_block.jpg" width="900"/>

*Controller Logic*

## Technical Components

### Thermocouple

### Power and Pulse Width Modulation (PWM)

### Voce Feedback

### User Interface

## Testing and Printed Circuit Board
