#include "echodef.h"

extern int accept_client(int);
extern int connect_peer(char *,char *);
extern int listen_to(char *);
extern void set_nonblock(int);

extern int ping(int);
extern int echo(int);

double timestamp(void);
