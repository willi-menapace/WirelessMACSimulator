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

import math
import copy
import random
from module import Module
from event import Event
from events import Events
from packet import Packet

class Channel(Module):
    """
    This class takes care of simulating the channel, signaling the beginning of
    reception to nodes within the communication range. For the sake of
    simplicity, the end of reception is automatically scheduled by the receiving
    node.
    """

    # communication range parameter in config file
    PAR_RANGE = "range"
    # speed of light in m/s, used to compute propagation delay
    SOL = 299792458.0

    def __init__(self, config):
        """
        Constructor.
        :param config: the set of configs loaded by the simulator to obtain, for
        example, the communication range. The parameter is an instance of the
        Config class
        """
        # call superclass constructor
        Module.__init__(self)
        # get transmission range from configuration parameters
        self.range = config.get_param(self.PAR_RANGE)
        # list of all communication nodes in the simulation
        self.nodes = []
        # map of neighbors that maps each node id to the list of its neighbors
        self.neighbors = {}

    def register_node(self, node):
        """
        Registers a node participating to the simulation. This way the channel
        knows who is participating and can notify them when transmissions start
        or end
        :param node: the node to register, an instance of the Node class
        """
        self.nodes.append(node)
        # recompute the neighbors of all nodes considering the new node as well
        self.recompute_neighbors(node)

    def distance(self, a, b):
        """
        Computes the two-dimensional Euclidean distance between nodes a and b
        :param a: first node
        :param b: second node
        """
        return math.sqrt(math.pow(a.get_posx() - b.get_posx(), 2) +
                         math.pow(a.get_posy() - b.get_posy(), 2))

    def recompute_neighbors(self, new_node):
        """
        Updates the map of neighbors, i.e., for each node it computes the list
        of nodes within communication range and stores such list
        :param new_node: the node just added to the simulation
        """
        # neighbors for the newest node
        new_node_neighbors = []
        for n in self.nodes:
            # if the node n is within communication range of the newest node
            if n.get_id() != new_node.get_id() and \
               self.distance(n, new_node) < self.range:
                # add n to the neighbors of the new node and vice versa
                new_node_neighbors.append(n)
                self.neighbors[n.get_id()].append(new_node)
        # save neighbors for the new node in the map
        self.neighbors[new_node.get_id()] = new_node_neighbors

    def start_transmission(self, source_node, packet=None):
        """
        Begins transmission of a frame on the channel, notifying all neighbors
        about such event
        :param source_node: node that starts the transmission
        :param packet: packet being transmitted
        """
        for neighbor in self.neighbors[source_node.get_id()]:
            node_distance = self.distance(source_node, neighbor)

            # compute propagation delay: distance / speed of light
            propagation_delay =  node_distance /\
                                Channel.SOL

            #Performs a copy so that packets can become corrupted independently in any station
            new_packet = copy.deepcopy(packet)

            #Packet gets corrupted with a probability that is linear to the distance
            #Corruption does not destroy the packet so that the receiving station still
            #senses energy in the air
            corruption_probability = node_distance / self.range
            result = random.uniform(0, 1)
            if result <= corruption_probability:
                new_packet.set_state(Packet.PKT_CORRUPTED)

            # generate and schedule START_RX event at receiver
            # be sure to make a copy of the packet and not pass the same
            # reference to multiple nodes, as they will process the packet in
            # different ways. one node might be able to receive it, one node
            # might not
            event = Event(self.sim.get_time() + propagation_delay,
                          Events.START_RX, neighbor, source_node,
                          new_packet)
            self.sim.schedule_event(event)
