#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#include "echo.h"

int
client(char *peer, int num) {
  int sock = connect_peer(peer, ECHO_PORT);
  int i;

  for (i = 0; i < num; i++) {
    double timestamp_before = timestamp();
    if (ping(sock) == 0) { break; }
    double timestamp_after = timestamp();
    printf("%i %lf\n", i, timestamp_after - timestamp_before);
  }

  return 0;
}

int
usage(int argc, char** argv) {
  char *prog = argv[0];
  fprintf(stderr, "usage: %s <server_addr> <#_of_pings>\n", prog);
  return -1;
}

int
main(int argc, char** argv) {
  if(argc != 3) {
    return usage(argc, argv);
  }
  char *peer  = argv[1];
  char *pings = argv[2];
  int num = 0;
  sscanf(pings, "%d", &num);
  return client(peer, num);
}
