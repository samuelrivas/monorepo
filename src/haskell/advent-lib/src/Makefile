all:
	cd src; ghc --make -dynamic -shared -fPIC -package-name adventlib System/IO/Advent.hs System/IO/Test.hs -osuf dyn_o -hisuf dyn_hi -o libHSadventlib-ghc8.6.5.so
	cd src; ghc -c --make -package-name adventlib System/IO/Advent.hs System/IO/Test.hs
	ar cqs src/libHSadventlib.a src/System/IO/*.o
	ghc --make src/MainTest.hs
	ghc --make -dynamic src/MainTest.hs -o src/MainTest_dyn

clean:
	find . -name "*.so" -delete
	find . -name "*.o" -delete
	find . -name "*.a" -delete
	find . -name "*.hi" -delete
	find . -name "*.dyn_*" -delete
	rm -f src/MainTest
	rm -f src/MainTest_dyn
