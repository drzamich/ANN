 set title "Cost function"
 set xrange[1:        5000 ]
 set yrange[   1.64490643E-12 :   1.49969822E-02 ]
 set xlabel "Step"
 set xtics 100
 set style data lines
 set output plot.png
 plot "cost.txt" title "Cost"
