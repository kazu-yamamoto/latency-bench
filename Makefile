MAKE=make

all:
	(cd lib;      $(MAKE))
	(cd single;   $(MAKE))
	(cd multiple; $(MAKE))

clean:
	(cd lib;      $(MAKE) clean)
	(cd single;   $(MAKE) clean)
	(cd multiple; $(MAKE) clean)
