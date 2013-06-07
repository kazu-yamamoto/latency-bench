#include <ev.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "echo.h"

void
event_echo(EV_P_ struct ev_io *w, int revents) {
  if (echo(w->fd) <= 0) {
    close(w->fd);
    ev_io_stop(EV_A_ w);
    free(w);
  }
}

void
event_server(EV_P_ struct ev_io *w, int revents) {
  int sock = accept_client(w->fd);
  set_nonblock(sock);

  ev_io *client_watcher = calloc(1, sizeof(ev_io));
  struct ev_loop *l = w->data;
  ev_io_init(client_watcher, event_echo, sock, EV_READ);
  ev_io_start(l, client_watcher);
}

int
server() {
  int listen_socket = listen_to_echo_port();
  set_nonblock(listen_socket);

  struct ev_loop *loop = ev_default_loop(0);
  ev_io watcher;
  watcher.data = loop;
  ev_io_init(&watcher, event_server, listen_socket, EV_READ);
  ev_io_start(loop, &watcher);
  ev_loop(loop, 0);

  close(listen_socket);
  return 0;
}

int
main() {
  printf("Echo server started.\n");
  server();
  /* never reached */
  printf("Echo server finished.\n");
  return 0;
}
