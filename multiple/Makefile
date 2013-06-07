PROGS=echo-server echo-nonthreaded-server echo-threaded-server
#PROGS=echo-server echo-client echo-nonthreaded-server echo-threaded-server

CC=gcc
CFLAGS=-Wall
HC=ghc
HCFLAGS=-O -Wall

all: $(PROGS)

echo-server: echo-server.c
	$(CC) $(CFLAGS) -o $@ $< -lev -lecho -L../lib -I../lib

# echo-client: echo-client.c
# 	$(CC) $(CFLAGS) -o $@ $<

echo-nonthreaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -o $@ $< -I../lib

echo-threaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -threaded -o $@ $< -I../lib

clean:
	rm -f $(PROGS) *.o *.hi
