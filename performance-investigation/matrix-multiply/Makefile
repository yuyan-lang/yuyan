

yuyan: 
	yy -c -O3 matrix-multiply.yuyan -o matrix_yuyan_o3.out


yuyandebug: 
	yy -c matrix-multiply.yuyan -o matrix_yuyan.out
yuyanprofile: 
	yy -c matrix-multiply.yuyan -o matrix_yuyan.out --profile


time_plot.out: time_plot.yuyan
	yy -c -o time_plot.out $^

time : time_plot.out
	./time_plot.out

sml : matrix-multiply.sml matrix-multiply.mlb
	mlton -output matrix_sml.out matrix-multiply.mlb

hs : matrix-multiply.hs
	stack ghc matrix-multiply.hs -- -O
	mv matrix-multiply matrix_hs.out
