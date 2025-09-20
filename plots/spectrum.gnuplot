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

set terminal svg size 225,150 dynamic enhanced
set output outfile
set datafile separator ','
set key off
set grid xtics ytics
set errorbars small
set xlabel "alpha" offset 0,0.7
set ylabel "f(alpha)" offset 2.2,0
set xrange [0:1.1]
set yrange [-0.4:1]
set tics out
set tics scale 0.4
plot x with lines dt 3 lc "black", infile using "alpha":"f" with lines lc "black"
