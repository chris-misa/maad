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

set terminal svg size 500,500 dynamic enhanced
set output outfile
set datafile separator ','
set key off
set errorbars small
set xlabel "q"
set ylabel "tauTilde(q)"
plot infile using "q":"tauTilde":(column("sd")*1.959964) with yerrorlines pointtype 0
# multiply sd by qnorm(0.975) for confidence interval