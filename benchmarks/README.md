# Codex Benchmarks

This folder contains various benchmarks for codex, as well as scripts and makefiles
to quickly run them.

## Benchmark suites

- `svbenchmarks4codex`: benchmarks from the software verification competition.
  In order to run these,
  1. Clone the `svbenchmarks4codex` submodule
     ```bash
     $ git submodule update --init
     ```
  2. Download the benchmark suite from https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks.
     By default, the script assumes these are installed in `/opt/git/sv-benchmarks`.
     If installed elsewhere, change the `SVCOMP_PATH` variable in [./manage.py](./manage.py)
     and the `SVCOMPPATH` in [./sv-benchmarks4codex/Makefile](./sv-benchmarks4codex/Makefile)

- `whole-program`: this contains hand-picked examples of programs to analyze.
  It also requires cloning the submodule to run.

- `csmith`: this allows running generating random program to run benchmarks on
  using the [csmith](https://github.com/csmith-project/csmith) tool.
  To generate programs, run:
  ```bash
  $ make csmith-forge -j 8
  ```
  You can customize the amount of programs generated (`CSMITH_AMOUNT`) and the
  generation flags (`CSMITH_FLAGS`) in the [Makefile](./Makefile).

  In order to run analysis on csmith files, we must pass the header to `frama-c`.
  For my install, they are situated in `/usr/include/csmith`. If they are somewhere
  else on you machine, update the `CSMITH_INCLUDE` variable in the [Makefile](./Makefile)
  accordingly.

- `type-benchmarks`: benchmarks run on the CI as tests, also as a submodule.
  These have their own system, so check out the included [types-benchmarks/Makefile](./types-benchmarks/Makefile)
  for how to run them. TODO: move them to this common system.

The `pycli` folder is just a python library for the `manage.py` script.

## The `manage.py` script

For the first three suites, the [manage.py](./manage.py) script offers various options to run
and analyze their results. See `./manage.py --help` (as well as `./manage.py subcommand --help`)
or the comment at the top of the file to get a feeling of the different sub-commands provided.
```bash
Usage: ./manage.py [--global-options] subcommand [--command-specific-options]
```

For a tldr of how to use it:
```bash
$ ./manage.py run -j 8 --flags ' -some-codex-options' # make sure to dune build and install before running!

$ ./manage.py list-ok # show stats about analyses that suceeded
$ ./manage.py list-error # show a "fail reason" for analyses that failed

$ ./manage.py (output|stats|log) path/to/file.c # show dump file/stats/stdout of the given file

$ ./manage.py save --name foo --comment "commit ffffffff with option -some-codex-option" # Create a named save

$ ./manage.py run -j 8 --flags ' -some-other-option'
$ ./manage.py save --name bar
$ ./manage.py compare foo bar # show a diff of both saves, file by file
$ ./manage.py constrast foo bar # show files which suceeded in only one of the saves
```
For the `list-ok` and `compare` sub-commands, the displayed columns can be changed
either by editing the `DEFAULT_COLUMNS` and `DEFAULT_DIFF_COLUMNS` in the file,
or using the `-c` command line option. For a list of possible columns, see
`./manage.py list-stats`.
