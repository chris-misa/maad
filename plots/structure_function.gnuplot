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
set arrow from (-1.0),-1.5 to (-1.0),1.5 nohead dashtype 2
set arrow from (3.4),-1.5 to (3.4),1.5 nohead dashtype 2
plot infile using "q":"tauTilde":(column("sd")*1.959964) with yerrorlines pointtype 0
# multiply sd by qnorm(0.975) for confidence interval