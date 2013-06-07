#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

#include "echo.h"

////////////////////////////////////////////////////////////////

void
exit_error(int ret, char *msg) {
  if (ret < 0) {
    perror(msg);
    exit(EXIT_FAILURE);
  }
}

void
exit_gai_error(int ret) {
  if (ret != 0) {
    fprintf(stderr, "%s\n", gai_strerror(ret));
    exit(EXIT_FAILURE);
  }
}

////////////////////////////////////////////////////////////////

struct addrinfo *
get_my_addr(char *port) {
  struct addrinfo hints, *addr;
  memset(&hints, 0, sizeof(hints));
  hints.ai_flags = AI_PASSIVE;
  hints.ai_family = AF_INET6;
  hints.ai_socktype = SOCK_STREAM;
  int ret = getaddrinfo(NULL, port, &hints, &addr);
  exit_gai_error(ret);
  return addr;
}

int
get_server_socket(struct addrinfo *addr) {
  int sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
  exit_error(sock, "socket");
  return sock;
}

int
get_socket() {
  int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  exit_error(sock, "socket");
  return sock;
}

void
bind_socket(int sock, struct addrinfo *addr) {
  int ret = bind(sock, addr->ai_addr, addr->ai_addrlen);
  exit_error(ret, "bind");
}

void
listen_socket(int sock, int qlen) {
  int ret = listen(sock, qlen);
  exit_error(ret, "socket");
}

void
set_reuseaddr(int sock) {
  int on = 1;
  int ret = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
  exit_error(ret, "setsockopt REUSEADDR");
}

void
set_nodelay(int sock) {
  int on = 1;
  int ret = setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &on, sizeof(on));
  exit_error(ret, "setsockopt NODELAY");
}

void
set_nonblock(int sock) {
  int flags = fcntl(sock, F_GETFL);
  int ret = fcntl(sock, F_SETFL, (flags | O_NONBLOCK));
  exit_error(ret, "NONBLOCK");
}

int
listen_to_echo_port () {
  struct addrinfo *addr = get_my_addr(ECHO_PORT);
  int sock = get_server_socket(addr);
  set_reuseaddr(sock);
  bind_socket(sock, addr);
  listen_socket(sock, MAX_BACKLOG);
  freeaddrinfo(addr);
  return sock;
}

////////////////////////////////////////////////////////////////

int
accept_client(int listen_socket) {
  int sock = accept(listen_socket, NULL, NULL);
  exit_error(sock, "accept");
  set_nodelay(sock);
  return sock;
}

////////////////////////////////////////////////////////////////

int
echo(int sock) {
  char buf[ECHO_SIZE];

  size_t rlen = recv(sock, buf, ECHO_SIZE, 0);
  exit_error(rlen, "recv()");

  if (rlen == 0) {
    return 0;
  }

  size_t slen = send(sock, buf, rlen, 0);
  exit_error(slen, "send()");

  return rlen;
}
