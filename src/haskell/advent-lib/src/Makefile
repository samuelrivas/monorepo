all:
	ghc -shared -dynamic-too -fPIC -package-name adventlib src/System/IO/*.hs  -o src/libHSadventlib-ghc8.6.5.so || true
	ghc -shared -dynamic -fPIC -package-name adventlib src/System/IO/*.hs  -o src/libHSadventlib-ghc8.6.5.so
	ar cqs libHSadventlib.a src/System/IO/*.o
clean:
	find . -name "*.so" -delete
	find . -name "*.o" -delete
	find . -name "*.a" -delete
	find . -name "*.hi" -delete
	find . -name "*.dyn_*" -delete
