# Plotting scripts

Run `make init` to build the docker container for plotting.

Place your `*.csv` files in the `data/` folder and load them in `instances.R`.
The files must at least have the following columns: `Graph`, `K`, `Cut`, `Imbalance` and `Time`.
Other supported columns are `Seed` (for repetitions), `Epsilon` (for epsilons other than 0.03), `Threads` (for speedup plots) and `Failed` (for crashes).

Run `make example-pdf` or `make example-tex` to generate example plots with the example data provided in the `data/` folder.
