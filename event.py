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

from events import Events


class Event:
    """
    Defines the basic structure of an event
    """

    def __init__(self, event_time, event_type, destination, source, obj=None):
        """
        Creates an event.
        :param event_time: time at which the event should be scheduled
        :param event_type: type of event
        :param destination: destination module that should be notified
        :param source: module generating the event
        :param obj: optional object to be attached to the event
        """
        self.event_time = event_time
        self.event_type = event_type
        self.destination = destination
        self.source = source
        self.obj = obj

    def get_time(self):
        """
        Returns event time
        """
        return self.event_time

    def get_type(self):
        """
        Returns event type
        """
        return self.event_type

    def get_destination(self):
        """
        Returns event destination
        """
        return self.destination

    def get_source(self):
        """
        Returns event generator
        """
        return self.source

    def get_obj(self):
        """
        Returns the object attached to the event
        """
        return self.obj

    def dump_event(self):
        """
        Prints the event in a human readable format
        """
        print("Event time: %f" % self.event_time)
        t = ""
        if self.event_type == Events.PACKET_ARRIVAL:
            t = "ARRIVAL"
        elif self.event_type == Events.START_TX:
            t = "START_TX"
        elif self.event_type == Events.START_RX:
            t = "START_RX"
        elif self.event_type == Events.END_TX:
            t = "END_TX"
        elif self.event_type == Events.END_RX:
            t = "END_RX"
        elif self.event_type == Events.END_PROC:
            t = "END_PROC"
        print("Event type: %s" % t)
        print("Source node: %d" % self.source.get_id())
        print("Destination node: %d\n" % self.destination.get_id())
