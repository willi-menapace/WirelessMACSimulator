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

import json
import re
import sys


class Config:
    """
    Reads simulation config from configuration file. The configuration file is
    in JSON format with the addition of text comments (non-standard JSON).
    Comments are automatically removed before processing the JSON content
    """

    # output file name parameter
    OUTPUT = "output"

    def __init__(self, config_file, section):
        """
        Constructor.
        :param config_file: file name of the config file
        :param section: the section of the configuration file to load
        """
        # save basic configuration
        self.config_file = config_file
        self.section = section
        # load configuration from json
        json_content = self.remove_comments(config_file)
        try:
            self.cfg = json.loads(json_content)
        except Exception as e:
            print("Unable to parse " + self.config_file)
            print(e.message)
            sys.exit(1)
        if section not in self.cfg:
            print("Error: the file %s does not contain section %s",
                  (config_file, section))
            sys.exit(1)
        # create the mapping between run numbers and parameters
        self.map_parameters()
        # set the run number to 0 by default
        self.run_number = 0
        # compute the output file name for run number 0
        self.compute_output_file_name()

    def map_parameters(self):
        """
        Creates a map from run number to the index of each parameter for that
        specific run number. For example, if we have parameters a and b, defined
        as a = [1, 2, 3] and b = [5, 6], we will need to run the following
        simulations
            a = 1, b = 5
            a = 1, b = 6
            a = 2, b = 5
            a = 2, b = 6
            a = 3, b = 5
            a = 3, b = 6
        so we will need to run 6 simulations. The run id goes from 0 to 5, and
        for each run id we map a particular tuple of parameters. What we do here
        is, given the run id, computing the index of a particular parameter
        inside its array of values. For example, for the aforementioned case
        this function computes
            run = 0, a = 0, b = 0
            run = 1, a = 1, b = 1
            run = 2, a = 2, b = 0
            run = 3, a = 0, b = 1
            run = 4, a = 1, b = 0
            run = 5, a = 2, b = 1
        """
        # compute the total number of runs. given that we are performing a
        # cartesian product, we simply multiply the sizes of all parameters
        # given as a list of values
        count = 1
        for p in self.cfg[self.section].keys():
            if type(self.cfg[self.section][p]) == list:
                count = count * len(self.cfg[self.section][p])

        # list of run ids, going from 0 to count - 1
        runs = range(count)
        # map from run number to index of a parameter inside its array
        par_map = {}
        prev_size = 1
        for p in self.cfg[self.section].keys():
            if type(self.cfg[self.section][p]) == list:
                own_size = len(self.cfg[self.section][p])
                par_map[p] = map((lambda x: x / prev_size % own_size), runs)
                prev_size = prev_size * own_size

        self.runs_count = count
        self.par_map = par_map

    def get_runs_count(self):
        """
        Returns the number of runs in the simulation
        :returns: count of runs
        """
        return self.runs_count

    def set_run_number(self, run_number):
        """
        Sets the simulation we want to run
        :param run_number: the simulation run
        """
        self.run_number = run_number
        self.compute_output_file_name()

    def remove_comments(self, json_file):
        """
        Removes the comments from a json file (non standard)
        :param json_file: json file name
        :returns: the content of the file without comments
        """
        # regular expression to remove comments from json file
        cr = re.compile('(^)?[^\S\n]*/(?:\*(.*?)\*/[^\S\n]*|/[^\n]*)($)?',
                        re.DOTALL | re.MULTILINE)
        content = ''.join(open(json_file).readlines())
        # looking for comments
        match = cr.search(content)
        while match:
            # single line comment
            content = content[:match.start()] + content[match.end():]
            match = cr.search(content)
        return content

    def get_param(self, param):
        """
        Returns the value of a parameter from the configuration file. Throws an
        error if the parameter is not found
        :param param: the parameter's name
        """
        # first check that param exists
        if param in self.cfg[self.section]:
            # if the parameter is in par_map, then it is a vector of values. In
            # such a case, we take a value depending on the run number
            if param in self.par_map:
                index = self.par_map[param][self.run_number]
                return self.cfg[self.section][param][index]
            # if instead the parameter is not in par_map, then it's a single
            # value. Just return it
            else:
                return self.cfg[self.section][param]
        else:
            print("Error: parameter %s not found in section %s",
                  (param, self.section))
            sys.exit(1)

    def compute_output_file_name(self):
        """
        Computes output file name. The user can specify an output file name with
        variables that point to parameters in the configuration file. For
        example, by specifying output = "file_{seed}.csv" and having the seed
        parameter configured as seed = [0, 1], will result in two output files
        (one per simulation run) names "file_0.csv" and "file_1.csv". In the
        case of random variables (e.g.,
        size = {"distribution" : "exp", "lambda" : 10}), one can use a
        distribution parameter by using, in this specific case, {size.lambda}.
        """
        # if the user doesn't specify an output file name, then the output file
        # name is simply given by section name and run number, which are unique
        # identifiers for each simulation run
        if Config.OUTPUT not in self.cfg[self.section]:
            self.output_file = "%s_%d.csv" % (self.section, self.run_number)

        # shorthand to the simulation configuration
        config = self.cfg[self.section]
        # get output parameter template as configured by the user
        template = config[Config.OUTPUT]
        # final output file name
        output = ""
        # states saying whether we are currently parsing normal text or the name
        # of a variable
        OUTVAR = 0
        INVAR = 1
        # at the beginning we are outsize of a variable
        state = OUTVAR
        # name of the variable we are currently parsing
        var_name = ""
        # loop through all characters
        for i in range(len(template)):
            if template[i] == '{':
                # if the character is { then a variable is starting
                if state == OUTVAR:
                    # if that is the case, we must be in the OUTVAR state
                    state = INVAR
                    var_name = ""
                else:
                    # if not, there is a syntax error like {{
                    print("Invalid syntax for %s" % template)
                    sys.exit(1)
            elif template[i] == '}':
                # if the character is }, then this is the end of a variable name
                if state == INVAR:
                    # if syntax is correct, we search for the value of the
                    # specified parameter

                    # first, split variables (a.b -> 'a', 'b')
                    variables = var_name.split('.')
                    # start with the first one
                    var = variables[0]
                    if var in self.par_map:
                        # if the variable is in the par_map, we need to get the
                        # correct instance depending on the run number
                        index = self.par_map[var][self.run_number]
                        obj = config[var][index]
                        # if the parameter value is an array, instead of taking
                        # its value we take its index
                        if isinstance(obj, list):
                            obj = index
                    else:
                        # otherwise we simply take the only value it has
                        obj = config[var]
                        # if the parameter value is an array, instead of taking
                        # its value we take its index
                        if isinstance(obj, list):
                            obj = 0

                    # now simply perform "introspection"
                    for var in variables[1:len(variables)]:
                        obj = obj[var]

                    # finally get the value
                    output = output + str(obj)
                    # and change state
                    state = OUTVAR
                else:
                    # if we are not in the INVAR state, then we have a syntax
                    # error like }}
                    print("Invalid syntax for %s" % template)
                    sys.exit(1)
            else:
                # this is neither a { nor a }. so it's a standard character
                if state == INVAR:
                    # if inside a variable, add character to variable name
                    var_name = var_name + template[i]
                else:
                    # otherwise add character to output file name
                    output = output + template[i]

        if state == INVAR:
            # if we are not in the OUTVAR state at the end of the template,
            # then there is a syntax error
            print("Invalid syntax for %s" % template)
            sys.exit(1)

        self.output_file = output

    def get_output_file(self):
        return self.output_file

    def get_params(self, run_number):
        """
        Returns a textual representation of simulation parameters for a given
        run number
        :param run_number: the run number
        :returns: textual representation of parameters for run_number
        """
        params = ""
        config = self.cfg[self.section]
        for par, val in self.par_map.iteritems():
            params += "%s: %s " % (par, str(config[par][val[run_number]]))
        return params
