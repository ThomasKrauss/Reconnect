my-caches
=========

This code system helps to define hierarchical caches.

I have only used such cache in relation to code systems. The hierarchy is thus always the following: system -> module -> action. In plain language, a system have modules which have actions.

The goal of all the hierarchical caches is to be printed out as JSON in several files that are watched by NodeJS serves which push any change to connected browsers. Such caches are defined for line and form counts, dependencies, compile problems and usage problems. All these information are updated in real time on the various web displays.

The two hard parts are updating an information inside these caches and refreshing all connected information.

For instance, you change the definition of a function and it goes from 5 lines to 10 lines of code. The appropriate cache is updated at the precise position identified by the name of the system, of the module and of the edited action. Then the cache automatically update the stats of the parent module and then of the parent system.

The hidden goal of it all is that no computation of information should be done when reaching the various displays, except of course computations about how to display the information.