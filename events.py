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


class Events:
    """
    Defines event types for the simulation.

    Ordering defines event priority. Lower values mean higher priority
    """

    #End of the listening period
    END_LISTENING = 0
    #Request to send a new packet from above
    NEW_PACKET = 1
    #End of transmission
    END_TX = 2
    #End of a slot time in the contention window
    END_SLOT = 3
    #End of reception of a packet in the channel
    #Its priority is higher then START_RX so that if two packets are transmitted
    #one immediately after the other then the end event gets handled first and a fake collision
    #is not generated
    END_RX = 4
    #A packet is incoming on the channel
    START_RX = 5

