#include <stdio.h>

#include "echo.h"

int server() {
  int listen_socket = listen_to(ECHO_PORT);
  int sock = accept_client(listen_socket);

  for (;;) {
    if (echo(sock) == 0) { break; }
  }
  return 0;
}

int
main() {
  printf("Echo server started.\n");
  server();
  printf("Echo server finished.\n");
  return 0;
}
