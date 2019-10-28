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
from module import Module
from distribution import Distribution
from event import Event
from events import Events
from packet import Packet
from random import randint


class Node(Module):
    """
    This class implements a node capable of communicating with other devices
    """

    # transmission speed parameter (bits per second)
    DATARATE = "datarate"
    # queue size
    QUEUE = "queue"
    # contention window size
    WINDOW_SIZE = "window_size"
    # channel listening time (seconds)
    LISTENING_TIME = "listening"
    # inter-arrival distribution (seconds)
    INTERARRIVAL = "interarrival"
    # packet size distribution (bytes)
    SIZE = "size"
    # processing time distribution (seconds)
    PROC_TIME = "processing"
    # max packet size (bytes)
    MAXSIZE = "maxsize"


    # list of possible states for this node
    IDLE = 0
    TX = 1
    RX = 2
    SLOTTING = 3
    LISTENING = 4

    def __init__(self, config, channel, x, y):
        """
        Constructor.
        :param config: the set of configs loaded by the simulator
        :param channel: the channel to which frames are sent
        :param x: x position
        :param y: y position
        """
        Module.__init__(self)

        #Number of slots in the contention window
        self.window_slots_count = config.get_param(Node.WINDOW_SIZE)
        #Duration in seconds of the channel listening period
        self.listening_duration = config.get_param(Node.LISTENING_TIME)
        #Duration in seconds of each slot
        self.slot_duration = self.listening_duration

        # load configuration parameters
        self.datarate = config.get_param(Node.DATARATE)
        self.queue_size = config.get_param(Node.QUEUE)
        self.interarrival = Distribution(config.get_param(Node.INTERARRIVAL))
        self.size = Distribution(config.get_param(Node.SIZE))

        self.proc_time = Distribution(config.get_param(Node.PROC_TIME))
        self.maxsize = config.get_param(Node.MAXSIZE)
        # queue of packets to be sent
        self.queue = []


        # current state
        self.state = None
        self.switch_state(Node.IDLE)

        # save position
        self.x = x
        self.y = y
        # save channel
        self.channel = channel

        #Number of packets being received
        self.packets_in_air = 0

        #Number of window slots we still have to wait before transmitting
        self.slot_countdown = 0

        #First packet in the current sequence of receiving packets
        self.rx_sequence_first_packet = None

        #Hook to events in the queue for future manipulation
        self.end_listenting_event_hook = None
        self.end_slot_event_hook = None

    """
    Changes state of the reception node
    Performs necessary logging
    
    :param new state: state to which to switch
    """
    def switch_state(self, new_state):

        self.state = new_state
        self.logger.log_state(self, self.state)



    """
    Enqueues a new packet in the send buffer. If the buffer is full the packet is dropped
    Performs necessary logging
    
    :param packet_size: integer representing the size of the packet to transmit
    
    """
    def enqueue_packet_size(self, packet_size):
        if len(self.queue) < self.queue_size:
            self.queue.append(packet_size)
            self.logger.log_queue_length(self, len(self.queue))
        else:
            self.logger.log_queue_drop(self, packet_size)

    """
    Dequeues a packet from the buffer. The buffer must be non empty
    Performs necessary logging

    """
    def dequeue_packet_size(self):
        if(len(self.queue) == 0):
            print("Node %d tried to dequeue a packet from an empty queue", self.get_id())
            sys.exit(1)

        packet_size = self.queue.pop(0)
        self.logger.log_queue_length(self, len(self.queue))

        return packet_size


    def initialize(self):
        """
        Initialization. Starts node operation by scheduling the first packet
        """
        self.schedule_next_packet_arrival()

    def handle_event(self, event):
        """
        Handles events notified to the node
        :param event: the event
        """
        if event.get_type() == Events.END_LISTENING:
            self.handle_end_listenting(event)
        elif event.get_type() == Events.NEW_PACKET:
            self.handle_new_packet(event)
        elif event.get_type() == Events.END_TX:
            self.handle_end_tx(event)
        elif event.get_type() == Events.END_SLOT:
            self.handle_end_slot(event)
        elif event.get_type() == Events.START_RX:
            self.handle_start_rx(event)
        elif event.get_type() == Events.END_RX:
            self.handle_end_rx(event)
        else:
            print("Node %d has received a notification for event type %d which"
                  " can't be handled", (self.get_id(), event.get_type()))
            sys.exit(1)

    """
    Schedules the arrival of the next packet
    """
    def schedule_next_packet_arrival(self):

        # extract random value for next arrival
        arrival = self.interarrival.get_value()
        # generate an event setting this node as destination
        event = Event(self.sim.get_time() + arrival, Events.NEW_PACKET,
                      self, self)
        self.sim.schedule_event(event)

    """
    Schedules the next end of contention window slot
    """
    def schedule_next_slot_end(self):
        end_slot_event = Event(self.sim.get_time() +
                               self.slot_duration, Events.END_SLOT,
                               self, self, None)
        self.end_slot_event_hook = end_slot_event  # Saves the event for future cancellation
        self.sim.schedule_event(end_slot_event)

    """
    Handles the request to transmit a new packet
    """
    def handle_new_packet(self, event):

        new_packet_size = self.size.get_value()
        self.logger.log_arrival(self, new_packet_size)

        #Enqueues or drops packet
        self.enqueue_packet_size(new_packet_size)
        self.schedule_next_packet_arrival()

        #If we are idle then start immediately channel listening, otherwise keep doing what we were doing
        if self.state == self.IDLE:
            self.switch_state(self.LISTENING)

            end_listening_event = Event(self.sim.get_time() +
                                   self.listening_duration, Events.END_LISTENING,
                                   self, self, None)
            self.end_listenting_event_hook = end_listening_event  # Saves the event for future cancellation
            self.sim.schedule_event(end_listening_event)


    """
    Handles the end of listenting event
    """
    def handle_end_listenting(self, event):
        #An end listenting event can arrive only if we were listening
        #Did you forget to cancel the event?
        assert(self.state == self.LISTENING)

        #If we are at this point no packets must be in the air and there must be something to transmit
        assert(len(self.queue) > 0)
        assert(self.packets_in_air == 0)

        #Channel is free, start transmisison
        self.switch_state(self.TX)
        self.transmit_next_packet()

    """
    Handles the end of a slot during a contention window
    """
    def handle_end_slot(self, event):

        #And end slotting event can arrive only if we are in a contention window
        #Did you forget to cancel the event?
        assert(self.state == self.SLOTTING)

        #A contention window cannot be open if there are pakets being received or there is nothing to transmit
        assert (len(self.queue) > 0)
        assert(self.packets_in_air == 0)

        #Counts down a slot and check if we can transmit
        self.slot_countdown -= 1
        if self.slot_countdown == 0:
            #We won contention and can start transmit
            self.switch_state(self.TX)
            self.transmit_next_packet()

        else:
            self.schedule_next_slot_end()

    """
    Handles the end of transmission
    """
    def handle_end_tx(self, event):

        #And end tx event can arrive only if we were transmitting something
        assert(self.state == self.TX)

        if len(self.queue) == 0:
            #No need to open contention window. Decide what to to based on the state of the channel

            if self.packets_in_air == 0:
                #Nothing left to do
                self.switch_state(self.IDLE)
            else:
                #The channel is busy, start listenting
                self.switch_state(self.RX)

        else:
            #Need to open a contention window.
            self.switch_state(self.SLOTTING)
            self.slot_countdown = randint(0, self.window_slots_count - 1)

            if self.slot_countdown == 0:
                # Must transmit immediately without listening. Nothing new can get corrupted
                self.switch_state(self.TX)
                self.transmit_next_packet()

            elif self.packets_in_air > 0:
                #We must wait at least a slot and realize the channel is busy. No need to schedule slot countdowns, go to RX
                self.switch_state(self.RX)

            else:
                #We must wait but for now the channel is free. Schedule next slot countdown
                assert(self.packets_in_air == 0)

                self.schedule_next_slot_end()


    """
    Handles the start of reception of a packet
    """
    def handle_start_rx(self, event):

        #There is one more receiving packet in the air
        self.packets_in_air += 1
        current_packet = event.get_obj()

        #Schedules the end of reception
        end_rx_event = Event(self.sim.get_time() + current_packet.get_duration(),
                       Events.END_RX, self, self, current_packet)
        self.sim.schedule_event(end_rx_event)

        if self.packets_in_air == 1:
            #It was the first packet
            self.rx_sequence_first_packet = current_packet

        else:

            #Other packets were in the air, the current and the first packet are corrupted
            #First packet might already be completely processed so it could be None
            if self.rx_sequence_first_packet is not None:
                self.rx_sequence_first_packet.set_state(Packet.PKT_CORRUPTED)
            current_packet.set_state(Packet.PKT_CORRUPTED)

        if self.state == self.IDLE:
            self.switch_state(self.RX)

        if self.state == self.LISTENING:
            assert(self.end_listenting_event_hook is not None)
            self.sim.cancel_event(self.end_listenting_event_hook)
            self.switch_state(self.RX)

        if self.state == self.SLOTTING:
            assert(self.end_slot_event_hook is not None)
            self.sim.cancel_event(self.end_slot_event_hook)
            self.switch_state(self.RX)

        elif self.state == self.TX:
            #If I am transmitting there is a collision
            current_packet.set_state(Packet.PKT_CORRUPTED)



    def handle_end_rx(self, event):

        assert(self.state == self.RX or self.state == self.TX)

        self.packets_in_air -= 1

        ended_packet = event.get_obj()

        #If I ended the reception of the first packet remove it
        if self.rx_sequence_first_packet is not None and self.rx_sequence_first_packet.get_id() == ended_packet.get_id():
            self.rx_sequence_first_packet = None

        #If the packet was not corrupted then mark it as correctly received
        if ended_packet.get_state() == Packet.PKT_RECEIVING:
            ended_packet.set_state(Packet.PKT_RECEIVED)
            assert(self.state == self.RX) #Cannot correctly receive anything if not in RX

        self.logger.log_packet(event.get_source(), event.get_destination(), ended_packet)

        if self.state == self.RX:
            if self.packets_in_air != 0:
                #Do nothing, must still rx
                pass

            elif self.packets_in_air == 0 and len(self.queue) == 0:
                #Nothing left to do
                self.switch_state(self.IDLE)

            elif self.packets_in_air == 0 and len(self.queue) >= 0:

                # Need to open a contention window.
                self.switch_state(self.SLOTTING)
                self.slot_countdown = randint(0, self.window_slots_count - 1)

                if self.slot_countdown == 0:
                    # Must transmit immediately without listening.
                    self.switch_state(self.TX)
                    self.transmit_next_packet()

                else:
                    # We must wait but for now the channel is free. Schedule next slot countdown
                    self.schedule_next_slot_end()

    """
    Sends the first packet in the buffer, schedules end of tx event and performs necessary logging.
    Must be called in TX state. Buffer must be not empty.
    """
    def transmit_next_packet(self):

        assert(self.state == self.TX)
        assert(len(self.queue) > 0)

        packet_size = self.dequeue_packet_size()
        duration = packet_size * 8 / self.datarate

        # transmit packet
        tx_packet = Packet(packet_size, duration)
        self.channel.start_transmission(self, tx_packet)
        # schedule end of transmission
        end_tx = Event(self.sim.get_time() + duration, Events.END_TX, self,
                       self, tx_packet)
        self.sim.schedule_event(end_tx)

    """
    Returns x position
    :returns: x position in meters
    """
    def get_posx(self):

        return self.x

    """
    Returns y position
    :returns: y position in meters
    """
    def get_posy(self):

        return self.y
