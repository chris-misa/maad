# MAAD

Source code for various Multifractal Address Anomaly Detection (MAAD) techniques.

For background see Misa et al., 2025: https://arxiv.org/pdf/2504.01374.

# Compile

Use nix:
```
$ nix-shell
$ ./compile.sh
```

Otherwise, find your own way to get `ghc` and required libraries listed in `shell.nix`, then `./compile.sh`.

# Usage

To read from stdin, use "-" as input filepath.

A couple example input files with lists of IPv4 addresses are included in `./test_data/` for testing.

## Singularities

Usage:
```
$ ./Singularities <number of anomalous addresses to report> <input filepath>
```

Assumes the input file is a list of IPv4 addresses in dotted-decimal format (e.g., 192.0.2.1) with one address per line.

Outputs the addresses with highest and loweset alpha(x) estimated using least-squares. See `Singularities.hs` for details.


## Structure Function

Usage:
```
$ ./StructureFunction <input filepath>
```

Assumes the input file is a list of IPv4 addresses in dotted-decimal format (e.g., 192.0.2.1) with one address per line.

Not really an anomaly detection method. Outputs the estimated structure function (tauTilde(q)) for the given list of addresses along with estimated standard deviations. See `StructureFunction.hs` for details.
