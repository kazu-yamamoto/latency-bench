PROGS=echo-server echo-nonthreaded-server echo-threaded-server
#PROGS=echo-server echo-client echo-nonthreaded-server echo-threaded-server

CC=gcc
CFLAGS=-Wall
LDFLAGS=-L../lib -L/usr/local/lib -lecho -lev
CPPFLAGS=-I../lib -I/usr/local/include
HC=ghc
HCFLAGS=-O -Wall -I../lib

all: $(PROGS)

echo-server: echo-server.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) $(CPPFLAGS)

# echo-client: echo-client.c
# 	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS) $(CPPFLAGS)

echo-nonthreaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -o $@ $<

echo-threaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -threaded -o $@ $<

clean:
	rm -f $(PROGS) *.o *.hi
