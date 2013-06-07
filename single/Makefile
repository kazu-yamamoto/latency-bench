PROGS=echo-server echo-client echo-nonthreaded-server echo-threaded-server

CC=gcc
CFLAGS=-Wall
HC=ghc
HCFLAGS=-O

all: $(PROGS)

echo-server: echo-server.c
	$(CC) $(CFLAGS) -o $@ $<

echo-client: echo-client.c
	$(CC) $(CFLAGS) -o $@ $<

echo-nonthreaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -o $@ $<

echo-threaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -threaded -o $@ $<

clean:
	rm -f $(PROGS) *.o *.hi
