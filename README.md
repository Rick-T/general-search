# general-search

This library was created to provide an collection of generic search algorithms.

It provides a general interface for implementing pathfinding algorithms in Haskell as well as some default implementations.
Right now only Dijkstra's algorithm, A-Star/A\* and depth-first search (DFS) are supported.

The goal is to extend the capabilities of the interface and provide more implementations for a wider range of algorithms.

# Using the library
If you just want to perform a simple search without really stepping into the library, take a look at the module Data.Search.Simple.
It contains simple default implementations for common search algorithms.

If you want a more customized search you can take a look at the example-implementations in the `bench`-directory.
Also you will find documentation for the different modules on Hackage.

# Performance
Performance is not a main goal of this library.
However, it already runs faster than other implementations that are available on Hackage (with a factor 2x to 5x).
The benchmarks from this package can be used to examine the performance of different implementations for different scenarios.

If you know of any more pathfinding implementations in Haskell please let me know.
I'd be happy to include them.

# Benchmarks
This library comes with a easily extendable set of benchmarks. Currently they are all based on the [Moving AI 2D Pathfinding Benchmarks](https://movingai.com/benchmarks/grids.html).
Running the benchmarks requires adding the resource-files (*.map and *.map.scen) from the Moving AI website into the corresponding directory in the `bench/resources/movingai/*` directory.
The maps are not (yet) included in this repository because I have not (yet) acquired permission to re-distribute them.

Example: For running the benchmark with the map "Lost Temple" from Warcraft 3 the map- and scenario-file should be downloaded from [here](https://movingai.com/benchmarks/wc3maps512/index.html) and placed into the directory with the following structure:
`bench/resources/movingai/warcraft_3/losttemple.map`
`bench/resources/movingai/warcraft_3/losttemple.map.scen`

The site also allows for downloading all the maps as *.zip file. These *.zip files can be directly extracted to the correct directory.

# NOTE
While the algorithms provided by this library are already functional (pun intended) this is still a WORK IN PROGRESS.
Right now only best-first (shortest path) algorithms are fully supported.
Constructive feedback as well as other contributions will be very appreciated.
