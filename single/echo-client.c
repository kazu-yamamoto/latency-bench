#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <math.h>

#include "echo.h"

double timestamp() {
  struct timeval tp;
  gettimeofday(&tp, NULL);
  return ((double) tp.tv_sec) * 1e6 + (double) tp.tv_usec;
}

int client(char *addr, int pings) {
  struct addrinfo hints, *res;
  int error, client_socket, i;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family   = PF_INET;
  hints.ai_socktype = SOCK_STREAM;

  error = getaddrinfo(addr, ECHO_PORT, &hints, &res);
  if(error) {
    fprintf(stderr, "client error: %s\n", gai_strerror(error));
    return -1;
  }

  client_socket = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if(client_socket < 0) {
    fprintf(stderr, "client error: could not create socket\n");
    return -1;
  }

  if(connect(client_socket, res->ai_addr, res->ai_addrlen) < 0) {
    fprintf(stderr, "client error: could not connect: %s\n", strerror(errno));
    return -1;
  }

  for(i = 0; i < pings; i++) {
    double timestamp_before = timestamp();

    send(client_socket, "ping123", 8, 0);

    char *buf = malloc(8);
    ssize_t read = recv(client_socket, buf, 8, 0);

    if(read == 0) {
      fprintf(stderr, "server exited prematurely\n");
      free(buf);
      break;
    }

    free(buf);

    double timestamp_after = timestamp();
    printf("%i %lf\n", i, timestamp_after - timestamp_before);
  }

  freeaddrinfo(res);
  return 0;
}

int usage(int argc, char** argv) {
  fprintf(stderr, "usage: %s <server_addr> <#_of_pings>\n", argv[0]);
  return -1;
}

int
main(int argc, char** argv) {
  if(argc != 3) {
    return usage(argc, argv);
  }
  int pings = 0;
  sscanf(argv[1], "%d", &pings);
  return client(argv[1],pings);
}
