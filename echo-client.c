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
  printf("starting client\n");

  struct addrinfo hints, *res;
  int error, client_socket, i;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family   = PF_INET;
  hints.ai_socktype = SOCK_STREAM;

  error = getaddrinfo(addr, ECHO_PORT, &hints, &res);
  if(error) {
    printf("client error: %s\n", gai_strerror(error));
    return -1;
  }

  client_socket = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if(client_socket < 0) {
    printf("client error: could not create socket\n");
    return -1;
  }

  if(connect(client_socket, res->ai_addr, res->ai_addrlen) < 0) {
    printf("client error: could not connect: %s\n", strerror(errno));
    return -1;
  }

  for(i = 0; i < pings; i++) {
    double timestamp_before = timestamp();

    send(client_socket, "ping123", 8, 0);

    char *buf = malloc(8);
    ssize_t read = recv(client_socket, buf, 8, 0);

    if(read == 0) {
      printf("server exited prematurely\n");
      free(buf);
      break;
    }

    // printf("client received '%s'\n", buf);
    free(buf);

    double timestamp_after = timestamp();
    fprintf(stderr, "%i %lf\n", i, timestamp_after - timestamp_before);
  }

  printf("client did %d pings\n", pings);

  freeaddrinfo(res);
  return 0;
}

int usage(int argc, char** argv) {
  printf("usage: %s <number of pings>\n", argv[0]);
  return -1;
}

int
main(int argc, char** argv) {
  if(argc != 2) {
    return usage(argc, argv);
  }
  return client(argv[1],10000);
}
