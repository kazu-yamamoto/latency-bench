#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>

#include "echo.h"

int
listen_to_echo_port () {
  struct addrinfo hints, *res;
  int error, sock;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family   = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags    = AI_PASSIVE;

  error = getaddrinfo(NULL, ECHO_PORT, &hints, &res);
  if(error) {
    printf("getaddrinfo(): %s\n", gai_strerror(error));
    return -1;
  }

  sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if(sock < 0) {
    printf("socket()\n");
    return -1;
  }

  int yes = 1;
  if(setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
    printf("setsockopt()\n");
    return -1;
  }

  if(bind(sock, res->ai_addr, res->ai_addrlen) < 0) {
    printf("bind()\n");
    return -1;
  }

  listen(sock, MAX_BACKLOG);

  freeaddrinfo(res);

  return sock;
}

int
accept_client(int listen_socket) {
  int sock;
  struct sockaddr_storage client_addr;
  socklen_t addr_size;

  sock = accept(listen_socket, (struct sockaddr *)&client_addr, &addr_size);
  return sock;
}

int
echo(int sock) {
  char buf[ECHO_SIZE];
  size_t len;

  if ((len = recv(sock, buf, ECHO_SIZE, 0)) < 0) {
    printf("recv()\n");
    return -1;
  }

  if (len == 0) {
    return 0;
  }

  if (send(sock, buf, len, 0) < 0) {
    printf("send()\n");
    return -1;
  }

  return len;
}
