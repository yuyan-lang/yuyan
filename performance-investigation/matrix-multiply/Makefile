
matrix_yuyan_o3.out: matrix-multiply.yuyan
	yy compileOnlyoptimizeO3 $^
	cp ./.yybuild/yyexe matrix_yuyan_o3.out

yuyan: matrix_yuyan_o3.out

matrix_yuyan.out: matrix-multiply.yuyan
	yy compileOnly $^
	cp ./.yybuild/yyexe matrix_yuyan.out

yuyandebug: matrix_yuyan.out


time_plot.out: time_plot.yuyan
	yy compileOnly $^
	cp ./.yybuild/yyexe time_plot

time : time_plot.out
	./time_plot

sml : matrix-multiply.sml matrix-multiply.mlb
	mlton -output matrix_sml.out matrix-multiply.mlb

hs : matrix-multiply.hs
	stack ghc matrix-multiply.hs -- -O
	mv matrix-multiply matrix_hs.out