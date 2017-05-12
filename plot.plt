 set title "Cost function"
 set xrange[1:      500000 ]
 set logscale y
 set xlabel "Step"
 set ylabel "Cost value"
 set style data lines
 set terminal png
 set output "plot.png"
 plot "cost.txt" title "Cost"
