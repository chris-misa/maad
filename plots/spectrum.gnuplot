#
# Use gnuplot to plot the output of ../Spectrum.hs
#
# Usage: gnuplot -e 'infile="<input csv fiel>";outfile="<output svg file"' spectrum.gnuplot
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
set xlabel "alpha"
set xrange [0:1]
set ylabel "f(alpha)"
set yrange [0:1]
plot infile using "alpha":"f" with lines, x with lines
