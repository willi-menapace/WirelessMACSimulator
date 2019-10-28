# WirelessMACSimulator
A Wifi-like MAC protocol simulator written in Python. Simulates and analyzes the performance of a given network topology under a set of protocol parameters. The project was written extending Michele Segata's Aloha protocol simulator as part of a course assignment.

Read our [report](https://github.com/willi-menapace/WirelessMACSimulator/blob/master/report.pdf)!

# Instructions

-PYTHON SIMULATOR assignment topology, realistic propagation

`python worker.py`

-PYTHON SIMULATOR assignment topology, base propagation

Comment out lines #113 and #114 from channel.py

`python worker.py`

-PYTHON SIMULATOR arbitraty topology

Modify config.json
Set in worker.py line #13 the desired range of simulation runs between the available one for the current configuration
Realistic propagation can be turned off as described in the precedent point

For the R scripts root privileges are required in order to install packages

-process.R

Run a version of the python simulator to produce data and then

`sudo R --no-save < process.R`

Elaboration of results different from the ones generated with the given config.json may require reconfiguration of the windows.sizes global variable.

-plotting.R

Requires both files produced by a run of process.R on the assigned topology with realistic propagation and with base propagation. Realistic propagation files must be renamed so that they begin with "realistic_".

Sample files, ready for execution, can be found in execute_plotting

`sudo R --no-save < plotting.R`

-model.evaluation.R

Requires files produced by a simulation run where all nodes are in mutual range. Only files relative to the maximum simulated lambda must be present.

Sample files are not provided due to their large size

`sudo R --no-save < model.evaluation.R`

-simple.linear.model.R

Requires intermediate files produced by process.R on a simulation run where all nodes are in mutual range.
The n.nodes and window.size variables must be set before running the script.

Sample files, ready for execution, can be found in execute_simple_linear_model

`sudo R --no-save < simple.linear.model.R`
