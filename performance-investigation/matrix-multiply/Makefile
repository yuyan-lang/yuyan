
matrix_yuyan_o3.out: matrix-multiply.yuyan
	yy compileOnlyoptimizeO3 $^
	cp ./.yybuild/yyexe matrix_yuyan_o3.out


time_plot.out: time_plot.yuyan
	yy compileOnly $^
	cp ./.yybuild/yyexe time_plot

time : time_plot.out
	./time_plot