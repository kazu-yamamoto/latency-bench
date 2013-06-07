#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>

#include "echo.h"

int server() {
  int listen_socket = listen_to_echo_port();
  int sock = accept_client(listen_socket);

  for(;;) {
    char* buf = malloc(8);
    ssize_t read = recv(sock, buf, 8, 0);
    if(read == 0) {
      free(buf);
      break;
    }
    send(sock, buf, 8, 0);
    free(buf);
  }

  return 0;
}

int
main(int argc, char** argv) {
  printf("Echo server started.\n");
  server();
  printf("Echo server finished.\n");
  return 0;
}
