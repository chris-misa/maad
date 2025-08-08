#
# Use gnuplot to plot the output of ../StructureFunction.hs
#
# Usage: gnuplot -e 'infile="<input csv fiel>";outfile="<output svg file"' structure_function.gnuplot
#

if (!exists("infile")) {
   print "Missing variable \"infile\" (hint: use -e \"infile=<FILENAME>\")"
   exit
}

if (!exists("outfile")) {
   print "Missing variable \"outfile\" (hint: use -e \"outfile=<FILENAME>\")"
   exit
}

set terminal svg size 300,200 dynamic enhanced
set output outfile
set datafile separator ','
set key off
set grid xtics ytics
set errorbars small
set xlabel "q"
set ylabel "tauTilde(q)"

stats infile using "tauTilde" prefix "A" nooutput

set arrow from (-1.0),A_min to (-1.0),A_max nohead dashtype 2
set arrow from (3.4),A_min to (3.4),A_max nohead dashtype 2

set arrow from (-1.0 / 2),A_min to (-1.0 / 2),A_max nohead dashtype 3
set arrow from (3.4 / 2),A_min to (3.4 / 2),A_max nohead dashtype 3

plot infile using "q":"tauTilde":(column("sd")*1.959964) with yerrorlines pointtype 0
# multiply sd by qnorm(0.975) for confidence interval