PROGS=echo-server echo-client echo-nonthreaded-server echo-threaded-server
OBJS=weighttp.o worker.o client.o

CC=gcc
CFLAGS=-Wall
LDFLAGS=-L../lib -L/usr/local/lib -lecho -lev
CPPFLAGS=-I../lib -I/usr/local/include
HC=ghc
HCFLAGS=-O -Wall -I../lib

all: $(PROGS)

echo-server: echo-server.o
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

echo-client: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) -lpthread

echo-nonthreaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -o $@ $<

echo-threaded-server: echo-server.hs
	$(HC) $(HCFLAGS) -threaded -o $@ $<

clean:
	rm -f $(PROGS) *.o *.hi

.c.o:
	$(CC) $(CFLAGS) -c $< $(CPPFLAGS)
