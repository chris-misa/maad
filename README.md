# MAAD

This repo contains a reference implementation of various methods for analyzing the multifractal scaling of measures over the IPv4 and IPv6 address spaces. The main entry point is [MAAD.hs](MAAD.hs) which includes options for estimating the structure function, generalized dimensions, multifractal spectrum, comparison test statistics, and other useful things. Other files are either utilities used in MAAD.hs, tools for generating example (mostly mono-fractal) sets of addresses for comparison, or one-shot stand-alone analysis scripts. Yes, eventually this will all get cleaned up.

The name MAAD comes from a core thesis that these numeric estimations of multifractal scaling can be used for a new kind of "spatial" anomaly detection in Internet traffic. See [MAAD_OVERVIEW.md](MAAD_OVERVIEW.md) for details. Of course, these tools may be useful for more that just anomaly detection.

For background see Misa et al., 2025: https://arxiv.org/pdf/2504.01374.

# Compile

Use nix:
```
$ nix-shell
$ ./compile.sh
```

This will build the main MAAD executable entry point and anything else uncommented in `compile.sh`.

Otherwise, find your own way to get `ghc` and required libraries listed in `shell.nix`, then `./compile.sh`.

# Usage

The main executable produced is called `MAAD`. Run `./MAAD --help` for detailed and up-to-date documentation.

General usage notes:

* To read from stdin, use `--input -`. By default a file with only one dotted-decimal IPv4 address (or, if `--ipv6` is given, one standard textual IPv6 address representation) on each line is expected. Otherwise specify `--csv` and use the other related controls to specify which column holds addresses and (optionally) which holds the measure for each address. If you don't specify a measure column, each address is assumed to have measure one which analyzes the structure of how many distinct addresses are in each address prefix.

* To write a single csv analysis to stdout, use `--output - --format csv`. Note that for the csv format, the output flag specifies a prefix (possibly including directory paths) for multiple output files (e.g., `OUTPUT_structure.csv`, `OUTPUT_metadata.csv`, etc.)  Because of this, writing to stdout is only allowed if you're only doing a single analysis (in which case the metadata output is skipped). Alternatively, with `--format json` all outputs are combined in a single json object.

* MAAD includes a method for automatically determining a sufficient sample size (i.e., how many distinct IP addresses are needed to produce a high-confidence estimate). This is turned off by default. To enable it, use the `--auto-stop` flag. Be careful to note that even with `--auto-stop` enabled, the current version of MAAD does _not_ complain if the file simply did not contain enough addresses. Rather, this is reported in the metadata output's `did_auto_stop` field.

A couple example input files with lists of IPv4 addresses are included in `./test_data/` for testing.

Example 1: write the structure function to stdout.
```
$ ./MAAD --structure --auto-stop --input test_data/caida_100k --output -
```

Example 2: compare the structure functions of two different samples and write the resulting p-value and some other stuff to stdout. (For these example files the p-value should basically be zero!)
```
$ ./MAAD --auto-stop --input test_data/caida_100k --compare test_data/uniform_100k --output -
```

Example 3: write the generalized dimensions, multifractal spectrum, and metadata to stdout as json.
```
$ ./MAAD --auto-stop -sd --input test_data/caida_100k --format json --output -
```

Example 4: write the same as above, but to separate csv files, namely `./caida_100k_dimensions.csv`, `./caida_100k_spectrum.csv`, and `./caida_100k_metadata.csv`.
```
$ ./MAAD --auto-stop -sd --input test_data/caida_100k --format csv --output ./caida_100k
```

WARNING: several different metadata fields that MAAD tracks internally have not yet been incorporated in the json output. Until this is fixed, prefer csv output format for the complete metadata.

# Visualization

Scripts to generate plots of the structure and spectrum functions are included in the `./plots/` directory along with a wrapper script `./plots/plot.sh` that takes care of calling gnuplot with the right arguments.

For example, if you've saved the spectrum function in `spec.csv`, you could generate a plot by running
```
$ ./plots/plot.sh plots/spectrum.gnuplot caida_100k_spectrum.csv spec.svg
```
