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

import random
import sys
import math


class Distribution:
    """
    Generic distribution class that implements different distributions depending
    on the parameters specified in a configuration
    """

    # distribution type field
    DISTRIBUTION = "distribution"
    # mean field
    MEAN = "mean"
    # lambda field
    LAMBDA = "lambda"
    # min field
    MIN = "min"
    # max field
    MAX = "max"
    # integer distribution field
    INT = "int"
    # constant random variable
    CONSTANT = "const"
    # uniform random variable
    UNIFORM = "unif"
    # exponential random variable
    EXPONENTIAL = "exp"

    def __init__(self, config):
        """
        Instantiates the distribution
        :param config: an object used for configuring the distribution in the
        format {"distribution":NAME,"par1":value[,"par2":value,...]}.
        Accepted values so far are:
        {"distribution" : "const", "mean" : value}, constant variable
        {"distribution" : "exp", "mean" : value}, exponential random variable
        with mean being 1/lambda. "lambda" : value can also be used
        {"distribution" : "unif", "min" : value, "max" : value}, uniform random
        variable between min and max
        """
        try:
            # find the correct distribution depending on the specified name
            if config[Distribution.DISTRIBUTION] == Distribution.CONSTANT:
                self.d = Const(config[Distribution.MEAN])
            elif config[Distribution.DISTRIBUTION] == Distribution.UNIFORM:
                integer = False
                try:
                    int_distribution = config[Distribution.INT]
                    if int_distribution == 1:
                        integer = True
                except Exception:
                    integer = False
                self.d = Uniform(config[Distribution.MIN],
                                 config[Distribution.MAX], integer)
            elif config[Distribution.DISTRIBUTION] == Distribution.EXPONENTIAL:
                integer = False
                try:
                    int_distribution = config[Distribution.INT]
                    if int_distribution == 1:
                        integer = True
                except Exception:
                    integer = False
                if Distribution.MEAN in config:
                    self.d = Exp(config[Distribution.MEAN], integer)
                else:
                    self.d = Exp(1.0/config[Distribution.LAMBDA], integer)
            else:
                print("Distribution error: unimplemented distribution %s",
                      config[Distribution.DISTRIBUTION])
        except Exception as e:
            print("Error while reading distribution parameters")
            print(e.message)
            sys.exit(1)

    def get_value(self):
        return self.d.get_value()


class Const:
    """
    Constant random variable
    """

    def __init__(self, value):
        """
        Constructor
        :param value: returned constant value
        """
        self.value = value

    def get_value(self):
        return self.value


class Uniform:
    """
    Uniform random variable
    """

    def __init__(self, min, max, integer=False):
        """
        Constructor
        :param min: minimum value
        :param max: maximum value
        :param integer: whether to use integer or floating point numbers
        """
        self.min = min
        self.max = max
        self.integer = integer

    def get_value(self):
        value = random.uniform(self.min, self.max)
        if self.integer:
            return round(value)
        else:
            return value


class Exp:
    """
    Exponential random variable
    """

    def __init__(self, mean, integer=False):
        """
        Constructor
        :param mean: mean value (1/lambda)
        :param integer: if set to true, random values are discretized with ceil
        """
        self.l = 1.0/mean
        self.integer = integer

    def get_value(self):
        if self.integer:
            return math.ceil(random.expovariate(self.l))
        else:
            return random.expovariate(self.l)
