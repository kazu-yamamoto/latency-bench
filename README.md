# latency-bench

Latency benchmark tool for Haskell.

## Building

In the top directory, type:

    % make

You should do both on client and server machines.

This Makefile is very ad hoc, sorry. You must use "gmake". On BSD
variants, type:

    % gmake MAKE=gmake

## A single connection

### server (172.16.1.1):

    % cd single
    % ./echo-server

There are three echo servers:

- echo-server: written in C
- echo-threaded-seerver: written in Haskell with the threaded RTS
- echo-nonthreaded-seerver: written in Haskell with the nonthreaded RTS

Echo servers uses 8001 port.

### client (172.16.1.2):

    % cd single
    % ./echo-client 172.16.1.1 100

The last argument is the number of ping. echo-client send 8byte pings.

## Multiple connections

### server (172.16.1.1):

    % cd single
    % ./echo-server

There are three echo servers:

- echo-server: written in C
- echo-threaded-seerver: written in Haskell with the threaded RTS
- echo-nonthreaded-seerver: written in Haskell with the nonthreaded RTS

Echo servers uses 8001 port.

### client (172.16.1.2):

    % cd multiple
    % ./echo-client -n 100000 -c 1000 -t 10 -k "http://172.16.1.1:8001/"

echo-client is based on weighttp.

- -n: the number of pings
- -c: the number of connections
- -t: the number of worker native threads
- -k: using persistent connection

echo-client sends 8 byte pings, not HTTP requests.

## Credits

Most parts are copied from:

- https://github.com/haskell-distributed/network-transport-tcp
- https://github.com/lighttpd/weighttp
- http://d.hatena.ne.jp/winebarrel/20080309/p2
