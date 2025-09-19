#
# Usage: gnuplot -e 'd="<YYYYMMDD>"' one_day.gnuplot
#

if (!exists("d")) {
   print "Missing variable \"d\" (hint: use -e \"d=<YYYYMMDD>\")"
   exit
}


set terminal svg size 600,200 dynamic enhanced
set output (d . ".svg")
set datafile separator ','
set key off
set grid xtics ytics
set errorbars small
set xlabel "time-of-day (h)" offset 0,0.5
set ylabel "alpha" offset 2,0

set cblabel "f(alpha)" offset -1,0

set xrange [0:24]
set tics out
set tics scale 0.4
set xtics 4

set view 0,0

set palette model HSV
set palette defined ( 0 1 1 1, 1 0 1 1 )

file(i) = sprintf("data/%s%02u%02u_spectrum.csv", d, i / 12, (i % 12) * 5)
plot for [i=0:287] file(i) using (i * 5.0 / 60.0):"alpha":"f" with lines lc palette z lw 1.5
