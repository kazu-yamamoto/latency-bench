PROGS=echo-server echo-client echo-nonthreaded-server echo-threaded-server

CC=gcc
CFLAGS=-Wall
HC=ghc
HCFLAGS=-O

all: $(PROGS)

echo-server: echo-server.c
	$(CC) $(CFLAGS) -o $@ $< -lecho -L../lib -I../lib

echo-client: echo-client.c
	$(CC) $(CFLAGS) -o $@ $< -I../lib

echo-nonthreaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -o $@ $< -I../lib/

echo-threaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -threaded -o $@ $< -I../lib/

clean:
	rm -f $(PROGS) *.o *.hi
