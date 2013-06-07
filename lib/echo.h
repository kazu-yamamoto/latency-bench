#define ECHO_PORT "8001"
#define MAX_BACKLOG 2048
#define ECHO_SIZE 8

extern int listen_to_echo_port ();
extern int accept_client(int);
extern int echo(int);
extern void set_nonblock(int);



