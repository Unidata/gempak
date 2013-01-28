#include <geminc.h>
#include <gemprm.h>

#ifdef UNDERSCORE
#define wait_time	wait_time_
#define set_sigs	set_sigs_
#endif

int AVAL;

void alarm_reset( int sig )
{

signal(sig, SIG_IGN);

switch(sig)
   {
   case SIGINT:
   	signal(SIGINT, SIG_DFL);
   	alarm(0);
   	AVAL = -1;
	break;
   }
}

void set_sigs()
{
(void) signal (SIGINT, alarm_reset);
(void) signal (SIGALRM, alarm_reset);
}

void wait_time (int *iper, int *iret)
{
int ier;
unsigned waittime;

set_sigs();

AVAL = 0;
waittime = 60 * (*iper);

printf("waiting %u seconds<ctrl-c to continue>\n",waittime);
alarm(waittime);
ier = pause();

*iret = AVAL;

}
