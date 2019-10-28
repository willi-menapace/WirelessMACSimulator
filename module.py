import sys
import sim


class Module:
    """
    Defines a generic simulation module, implementing some basic functionalities
    that all modules should inherit from
    """

    # static class variable automatically incremented everytime a new module is
    # instantiated
    __modules_count = 0

    def __init__(self):
        """
        Constructor. Gets simulation instance for scheduling events and
        automatically assigns an ID to the module
        """
        self.sim = sim.Sim.Instance()
        # auto assign module id
        self.module_id = Module.__modules_count
        Module.__modules_count = Module.__modules_count + 1
        # get data logger from simulator
        self.logger = self.sim.get_logger()

    def initialize():
        """
        Initialization method called by the simulation for each newly
        instantiated module
        """
        return

    def handle_event(self, event):
        """
        This function should be overridden by inheriting modules to handle
        events for this module. If not overridden, this method will throw an
        error and stop the simulation
        """
        print("Module error: class %s does not override handle_event() method",
              self.get_type())
        sys.exit(1)

    def get_id(self):
        """
        Returns module id
        """
        return self.module_id

    def get_type(self):
        """
        Returns module type
        """
        return self.__class__.__name__
