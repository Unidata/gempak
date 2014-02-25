#include <signal.h>
#include <stdio.h>

#define TIMEOUT 600

static timeout_flag=0;

static void alarm_handler()
{
    unotice("SIGALRM: no data, returning EOF");
    timeout_flag++;
}

int getbuf(int unit, char *buf, int nbyte)

{
    static char initialized = 0;
    char *p = buf;
    int n = 0, status, i;

    if (initialized) {
        status  = 0;
    } else {
        struct sigaction        newact;
        struct sigaction        oldact;

        newact.sa_handler       = alarm_handler;
#       ifdef SA_INTERRUPT
            newact.sa_flags     = SA_INTERRUPT; /* for POSIX.1 semantics */
#       else
            newact.sa_flags     = 0;
#       endif
        (void) sigemptyset(&newact.sa_mask);

        if (-1 == sigaction(SIGALRM, &newact, &oldact)) {
            serror("getbuf: can't install SIGALRM handler");
	    exit(2);
        } else {
            if (SIG_DFL != oldact.sa_handler && SIG_IGN != oldact.sa_handler) {
                uerror("getbuf: SIGALRM handler already installed");
		exit(2);
            } else {
                initialized     = 1;
                status          = 0;
            }
        }
    }
    if(timeout_flag) return(0);
    while (n < nbyte && !timeout_flag) {

	alarm(TIMEOUT);
	if ((i=read(unit, p, nbyte-n)) > 0) {
	    n += i;
	    p += i;
	}
	else if (i == 0) {
	    break;
	}
	else {
	    serror("Read error - getbuf");
	    break;
	}
    }
    alarm(0);
    return(n);
}
