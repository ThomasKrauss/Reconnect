my-graphs
=========

This is my own library about graphs (currently under heavy refactoring; full rewrite would be more accurate).

To layout graphs, it relies on dot from <a href="http://www.graphviz.org/">Graphviz</a>. But it can be asked to perform the two following operations: sort nodes topologically and find the connected components of a graph.

Note that while it contains normal graph (which I called strict graphs), it also have lenient graphs. A lenient graph may have edges with one of their nodes not referenced in the graph. It is useful when dealing with cluster and links for one node in a cluster to a node in another cluster.

The way to see this situation is that a graph is materially a collection of links (the edges) bundled with a particular perspective of looking at them (the nodes) and these two things may not matched precisely. When they do, the graph is strict.