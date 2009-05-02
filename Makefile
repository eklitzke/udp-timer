timer: timer.hs UdpTimer/Globals.hs UdpTimer/Util.hs
	ghc --make -threaded timer.hs

clean:
	rm -f *.hi *.o
	rm -f UdpTimer/*.hi UdpTimer/*.o
