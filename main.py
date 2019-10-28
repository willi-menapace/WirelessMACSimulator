#!/usr/bin/env python
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright (C) 2016 Michele Segata <segata@ccs-labs.org>


from optparse import OptionParser
import sys
import sim

# setup command line parameters
parser = OptionParser(usage="usage: %prog [options]",
                      description="Runs a simulation configured in the "
                                  "specified config file under the specified "
                                  "section")
parser.add_option("-l", "--list", dest="list", default=False,
                  action="store_true", help="list the available runs and exit")
parser.add_option("-L", "--LIST", dest="verbose_list", default=False,
                  action="store_true", help="list the available runs with "
                                            "simulation parameters and exit")
parser.add_option("-r", "--run", dest="run", default=0, action="store",
                  help="run simulation number RUN [default: %default]",
                  metavar="RUN", type="int")
parser.add_option("-c", "--config", dest="config", default="config.json",
                  action="store",
                  help="simulation config file [default: %default]")
parser.add_option("-s", "--section", dest="section", default="simulation",
                  action="store",
                  help="section inside configuration file [default: %default]")

# parse options
(options, args) = parser.parse_args()

if options.config == "" or options.section == "":
    print("Required parameters config and section missing")
    print(parser.get_usage())
    sys.exit(1)

simulator = sim.Sim.Instance()
simulator.set_config(options.config, options.section)

# list simulation runs and exit
if options.list or options.verbose_list:
    runs_count = simulator.get_runs_count()
    for i in range(runs_count):
        if options.list:
            print("./main.py -c %s -s %s -r %d" %
                (options.config, options.section, i))
        else:
            print("./main.py -c %s -s %s -r %d: %s" %
                (options.config, options.section, i, simulator.get_params(i)))
    sys.exit(0)

simulator.initialize(options.run)
simulator.run()
