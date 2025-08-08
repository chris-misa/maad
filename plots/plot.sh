#!/usr/bin/env bash

if [ $# -ne 3 ]
then
    echo "Usage: <plot script name> <input file> <output file>"
    exit
fi


PLOT_SCRIPT=$1
INFILE=$2
OUTFILE=$3

gnuplot -e "infile=\"${INFILE}\";outfile=\"${OUTFILE}\"" ${PLOT_SCRIPT}

