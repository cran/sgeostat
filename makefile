
all:
	(cd .. ; R INSTALL -l $(R_LIB) sgeostat)

clean:
	rm -f src/*.o src/*/*.o src/*/*/*.o src/*.so

tar:
	(cd .. ; R CMD build sgeostat)
