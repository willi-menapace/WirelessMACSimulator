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

import sys
import heapq
import random
import time
import math
from singleton import Singleton
from config import Config
from channel import Channel
from node import Node
from log import Log

# VT100 command for erasing content of the current prompt line
ERASE_LINE = '\x1b[2K'


@Singleton
class Sim:
    """
    Main simulator class
    """

    # name of the section in the configuration file that includes all simulation
    # parameters
    PAR_SECTION = "Simulation"
    # simulation duration parameter
    PAR_DURATION = "duration"
    # seed for PRNGs
    PAR_SEED = "seed"
    # position of the nodes
    PAR_NODES = "nodes"

    def __init__(self):
        """
        Constructor initializing current time to 0 and the queue of events to
        empty
        """
        # current simulation time
        self.time = 0
        # queue of events, implemented as a heap
        self.queue = []
        # list of nodes
        self.nodes = []
        # initialize() should be called before running the simulation
        self.initialized = False
        # empty config file
        self.config_file = ""
        # empty section
        self.section = ""

    def set_config(self, config_file, section):
        """
        Set config file and section
        :param config_file: file name of the config file
        :param section: the section within the config file
        """
        self.config_file = config_file
        self.section = section
        # instantiate config manager
        self.config = Config(self.config_file, self.section)

    def get_runs_count(self):
        """
        Returns the number of runs for the given config file and section
        :returs: the total number of runs
        """
        if self.config_file == "" or self.section == "":
            print("Configuration error. Call set_config() before "
                  "get_runs_count()")
            sys.exit(1)
        return self.config.get_runs_count()

    def initialize(self, run_number):
        """
        Simulation initialization method
        :param run_number: the index of the simulation to be run
        """
        if self.config_file == "" or self.section == "":
            print("Configuration error. Call set_config() before initialize()")
            sys.exit(1)
        # set and check run number
        self.run_number = run_number
        if run_number >= self.config.get_runs_count():
            print("Simulation error. Run number %d does not exist. Please run "
                  "the simulator with the --list option to list all possible "
                  "runs" % run_number)
            sys.exit(1)
        self.config.set_run_number(run_number)
        # instantiate data logger
        self.logger = Log(self.config.get_output_file())
        # get simulation duration
        self.duration = self.config.get_param(self.PAR_DURATION)
        # get seeds. each seed generates a simulation repetition
        self.seed = self.config.get_param(self.PAR_SEED)
        random.seed(self.seed)
        # instantiate the channel
        self.channel = Channel(self.config)
        # instantiate all the nodes
        positions = self.config.get_param(self.PAR_NODES)
        for p in positions:
            x = p[0]
            y = p[1]
            node = Node(self.config, self.channel, x, y)
            # let the channel know about this node
            self.channel.register_node(node)
            node.initialize()
            self.nodes.append(node)
        # all done. simulation can start now
        self.initialized = True

    def get_logger(self):
        """
        Returns the data logger to modules
        """
        return self.logger

    def get_time(self):
        """
        Returns current simulation time
        """
        return self.time

    def schedule_event(self, event):
        """
        Adds a new event to the queue of events
        :param event: the event to schedule
        """
        if event.get_time() < self.time:
            print("Schedule error: Module with id %d of type %s is trying to "
                  "schedule an event in the past. Current time = %f, schedule "
                  "time = %f", (event.get_source.get_id(),
                                event.get_source.get_type(),
                                self.time,
                                event.get_time()))
            sys.exit(1)
        #Event type is pushed to establish event priority
        heapq.heappush(self.queue, (event.get_time(), event.get_type(), event))

    def next_event(self):
        """
        Returns the first event in the queue
        """
        try:
            event = heapq.heappop(self.queue)
            self.time = event[0]
            return event[2]
        except IndexError:
            print("No more events in the simulation queue. Terminating.")
            sys.exit(0)

    def cancel_event(self, event):
        """
        Deletes a scheduled event from the queue
        :param event: the event to be canceled
        """
        try:
            self.queue.remove((event.get_time(), event.get_type(), event))
            heapq.heapify(self.queue)
        except ValueError:
            print("Trying to delete an event that does not exist.")
            sys.exit(1)

    def run(self):
        """
        Runs the simulation.
        """
        # first check that everything is ready
        if not self.initialized:
            print("Cannot run the simulation. Call initialize() first")
            sys.exit(1)
        # save the time at which the simulation started, for statistical purpose
        start_time = time.time()
        # last time we printed the simulation percentage
        prev_time = start_time
        # print percentage for the first time (0%)
        self.print_percentage(True)
        # main simulation loop
        while self.time <= self.duration:
            # get next event and call the handle method of the destination
            event = self.next_event()
            dst = event.get_destination()
            dst.handle_event(event)
            # get current real time
            curr_time = time.time()
            # if more than a second has elapsed, update the percentage bar
            if curr_time - prev_time >= 1:
                self.print_percentage(False)
                prev_time = curr_time
        # simulation completed, print the percentage for the last time (100%)
        self.print_percentage(False)
        # compute how much time the simulation took
        end_time = time.time()
        total_time = round(end_time - start_time)
        print("\nMaximum simulation time reached. Terminating.")
        print("Total simulation time: %d hours, %d minutes, %d seconds" %
              (total_time / 3600, total_time % 3600 / 60,
               total_time % 3600 % 60))

    def print_percentage(self, first):
        # go back to the beginning of the line
        if not first:
            sys.stdout.write('\r' + ERASE_LINE)
        # compute percentage
        perc = min(100, int(math.floor(self.time/self.duration*100)))
        # print progress bar, percentage, and current element
        sys.stdout.write("[%-20s] %d%% (time = %f, total time = %f)" %
                         ('='*(perc/5), perc, (self.time), self.duration))
        sys.stdout.flush()

    def get_params(self, run_number):
        """
        Returns a textual representation of simulation parameters for a given
        run number
        :param run_number: the run number
        :returns: textual representation of parameters for run_number
        """
        return self.config.get_params(run_number)
