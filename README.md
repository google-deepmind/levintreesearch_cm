# Levin Tree Search with Context Models

Code for the paper "Levin Tree Search with Context Models" (IJCAI 2023, 
[extended version](https://arxiv.org/abs/2305.16945)).

This code allows to reproduce the results of the paper. 
Please report any issue, or contact lorseau@google.com if you need help.

## Installation

First, install the [**Racket**](https://racket-lang.org/) programming language
(Apache-2.0/MIT).
Racket version *8.9.0.4* or later is required, and at the time of writing
the latest *stable* version is only 8.9, 
so a [snapshot](https://users.cs.utah.edu/plt/snapshots/) build is required —
if in doubt, pick the *shortest name* in the `Racket` section that fits your
machine.

*Note for Linux users:*
Right click then `Save as...` and save the file in your `Downloads` folder.
Then in a terminal, do `cd Downloads` then `sudo sh racket-….sh` then reply `y`
to the first question (unix-like distribution) then press Enter for all
subsequent questions.

*Note for Windows Windows and MacOS users:*
[configure the PATH environment variable](https://github.com/racket/racket/wiki/Configure-Command-Line-for-Racket)
to include the directory containing the `racket` and `raco` executables.

On the command line, enter `racket -v` to check that version number is at least
8.9.0.4.

Then install the `levintreesearch_cm` package and all
its dependencies (all are Apache2 or Apache2/MIT licensed):

```shell
raco pkg install --auto --update-deps https://github.com/deepmind/levintreesearch_cm.git
```

## Quick start

On the command line, type

```shell
racket -l- lts-cm/domains/sokoban/example-optim-gui
```
This 
* runs the solver on a handful of simple sokoban levels, and displays
  the number of expansions (search steps) during search
* collects the solution trajectories,
* optimizes the parameters of the policy for these trajectories,
* runs the solver again and shows that the number of expansions has 
  reduced,
* opens a graphical interface to visualize one of the
  solution trajectories.

## Quick start 2: Server example

The previous example uses only one CPU.
Here's an example that runs several jobs in parallel using a server-worker
architecture on a set of easy Sliding Tile Puzzle instances:

```shell
racket -l- lts-cm/domains/stp/server-easy
```

With a 4-core machine, this takes less than 10 minutes to finish.

Each time a problem is solved, a file is written in the logs directory.
This is to avoid restarting from scratch in case the process is interrupted.
By default, all logs are stored in a subdirectory named `lts-cm-logs`
of the user's home directory.
This can be changed with the flag `--log-base-dir <dir>`.

By default, the number of workers is the number of cpus/2 (since often there 
are only half as many cpu cores as there are cpu threads). This can be changed
with `--n-workers` for search, and `--n-futures` for optimization.

Use the `--help` flag for more information.

## Reproducing the paper's results

NOTICE: Currently, reproducing the paper's results requires up to 2GB 
of RAM per worker, and up to 5GB of free disk space per experiment.

By increasing order of total computation time, here are all the commands to 
reproduce the results of the paper (possibly with some small differences):

```shell
racket -l- lts-cm/domains/witness/server
racket -l- lts-cm/domains/sokoban/server
racket -l- lts-cm/domains/stp/server
racket -l- lts-cm/domains/sokoban/server-long
racket -l- lts-cm/domains/rubiks-cube/server
```

The first three commands take less than 2 hours each to complete on a 64-core 
at 2GHz machine, while the last one takes several days in total (but just a few 
hours to obtain a first decent policy).

Note that the results may vary slightly depending on the value passed to
`--n-futures`.

## Dependencies

`levintreesearch_cm` requires Racket version *8.9.0.4* or later
(Apache 2.0/MIT), and depends on a few libraries that are licensed with 
Apache-2.0/MIT.
These dependencies are installed semi-automatically when following the
installation instructions.

## Datasets

The datasets for The Witness and Sliding-Tile Puzzle come from
[h-levin](https://github.com/levilelis/h-levin) (Apache-2.0 license).
The datasets for Sokoban come from
[Boxoban](https://github.com/deepmind/boxoban-levels)
(Apache-2.0 license).
The included Rubik's cube datasets are new.

## Citing this work

```
@inproceedings{orseau2023lts_cm,
  author    = {Laurent Orseau and Marcus Hutter and Levi H.S. Lelis},
  title     = {Levin Tree Search with Context Models},
  booktitle = {Proceedings of the Thirty-Second International 
               Joint Conference on Artificial Intelligence},
  pages     = {--},
  year      = {2023}
}
```

## License and disclaimer

Copyright 2023 DeepMind Technologies Limited

All software is licensed under the Apache License, Version 2.0 (Apache 2.0);
you may not use this file except in compliance with the Apache 2.0 license.
You may obtain a copy of the Apache 2.0 license at:
https://www.apache.org/licenses/LICENSE-2.0

All other materials are licensed under the Creative Commons Attribution 4.0
International License (CC-BY). You may obtain a copy of the CC-BY license at:
https://creativecommons.org/licenses/by/4.0/legalcode

Unless required by applicable law or agreed to in writing, all software and
materials distributed here under the Apache 2.0 or CC-BY licenses are
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
either express or implied. See the licenses for the specific language governing
permissions and limitations under those licenses.

This is not an official Google product.

