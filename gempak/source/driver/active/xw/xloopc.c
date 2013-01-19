#include "xwcmn.h"

/* 
 * definition of animation commands.
 */
#define GLOOP    1
#define GSTEP    3
#define GBACK    4
#define GREVERSE 5
#define GROCK    6

/* 
 * base value for dwell setting commands.
 */
#define DWELL_COMMAND    100
	

void xloopc ( int *number, int *iret )
/************************************************************************
 * xloopc								*
 *									*
 * This subroutine manages animation.					*
 *									*
 * xloopc  ( number, iret )						*
 *									*
 * Output parameters:							*
 *	*number	        int        command                         	*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * C. Lin/EAI	         8/94	Animation				*
 * C. Lin/EAI		 8/94	Changed calls to XXFLSH			*
 * C. Lin/EAI		10/94	Use select				*
 * C. Lin/EAI		 2/95	Error handling				*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 ***********************************************************************/
{
    int		command, com, ier, current_pxm, total_pxm, raise;
    Window_str	*cwin;
    Window	gwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    command = *number;

    cwin  = &(gemwindow[current_window]);
    gwin        = cwin->window;
    current_pxm = cwin->curpxm[cwin->curr_loop];
    total_pxm   = cwin->npxms;

    switch ( command ) {

      case GLOOP:
	LOOP = 1;
	com = GSTEP;

	/* 
	 * select the event for stopping the loop.
	 */

	XSelectInput ( gemdisplay, gwin, ButtonPressMask );

	looper( gwin, com );

	/* 
	 * reset the event selection.
	 */

	XSelectInput ( gemdisplay, gwin, ExposureMask); 
	break;

      case GSTEP:

	/*
	 * display the next picture
	 */

	cwin->curpxm[cwin->curr_loop] = (current_pxm + 1) % total_pxm;

	raise = G_FALSE;
	xxflsh( &raise, &ier );

	break;

      case GBACK:

	/*
	 * display the previous picture
	 */

	cwin->curpxm[cwin->curr_loop] = 
	    ( current_pxm + total_pxm - 1 ) % total_pxm;

	raise = G_FALSE;
	xxflsh( &raise, &ier );
	break;

      case GREVERSE:

	LOOP = 1;
	com = GBACK;

	/* 
	 * select the event for stopping the loop.
	 */

	XSelectInput ( gemdisplay, gwin, ButtonPressMask);

	looper( gwin, com );

	/* 
	 * reset the event selection.
	 */

	XSelectInput ( gemdisplay, gwin, ExposureMask); 
	break;

      default:

	/*
	 * check to see if it is a dwell rate command.
	 */

	com = command - DWELL_COMMAND;

	if ( ( com > 0 ) && ( com <= 5 ) )
	    dwell_rate = dwell[com - 1]*1000; 
				/* in milliseconds */

	break;
    }
}

/*=====================================================================*/

void looper ( Window gwin, int command )
/************************************************************************
 * looper								*
 *									*
 * This subroutine initiates forward or backward looping.		*
 *									*
 * looper  ( gwin, command )						*
 *									*
 * Input parameters:							*
 *	gwin		Window        window ID                         *
 *	command		int	      command code			*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	         8/94		animation			*
 * C. Lin/EAI	        10/94		pause on last frame		*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 ***********************************************************************/
{
    int		com, iret, nevntq;
    long int	wait;
    XEvent	gemevent;
    Window_str	*cwin;

    struct timeval  current;
    struct timezone tzp;

/*---------------------------------------------------------------------*/

    com = command;
    cwin  = &(gemwindow[current_window]);

    while ( LOOP )  {

	/*
	 * display the next pixmap in the loop.
	 */

	xloopc( &com, &iret );

	/*
	 * record the current time for later use in delay.
	 */

	gettimeofday( &current, &tzp );

	/*
	 * check events.
	 */

	nevntq = XEventsQueued ( gemdisplay, QueuedAfterReading );

	while ( nevntq-- ) {

	    /*
	     * Get the next event.
	     */

	    XNextEvent ( gemdisplay, &gemevent );

	    /*
	     * look for left mouse clicking event.
	     */

	    if (  ( gemevent.xbutton.window == gwin) &&
		  (gemevent.type == ButtonPress) && 
		  ( gemevent.xbutton.button == Button1 ) ){

		LOOP = 0;
		break;
	    }
	}

	if ( LOOP ) { /* no mouse click was caught */

	    /* 
	     * wait dwell_rate seconds from "current" time.
	     */

	    if ( cwin->curpxm[cwin->curr_loop] == (cwin->npxms - 1) ) {
		wait = 2*dwell_rate;
		delay( current, wait );
	    } 
	    else { 
		delay( current, dwell_rate );
	    }

	    looper( gwin, com );
	}
    }

}

/*=====================================================================*/

void delay ( struct timeval start_time, long interval )
/************************************************************************
 * delay								*
 *									*
 * This subroutine delays a program by "interval" milliseconds  	*
 *	counting from "start_time".					* 
 *									*
 * void delay  ( start_time, interval )					*
 *									*
 * Input parameters:							*
 *	start_time	struct timeval        base time 		*
 *	interval	long 		      delay time interval	*
 *					      in milliseconds		*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	         8/94		animation			*
 * C. Lin/EAI	         10/94		fix busy waiting by use select	*
 ***********************************************************************/
{
long int time;
struct timeval  now, timeout;
struct timezone tzp;

/*---------------------------------------------------------------------*/

	gettimeofday( &now, &tzp );

	time = interval - lapse( start_time, now );
	timeout.tv_sec = time/1000;
	timeout.tv_usec = (time%1000)*1000;

	select( 0, NULL, NULL, NULL, &timeout);

}

/*======================================================================*/

long lapse ( struct timeval time0, struct timeval time1 )
/************************************************************************
 * lapse								*
 *									*
 * This subroutine calculate the time difference in milliseconds of the *
 *	two input timeval structures.    				*
 *									*
 * long lapse  ( time0, time1 )						*
 *									*
 * Input parameters:							*
 *	time0	struct timeval        input timeval structure 		*
 *	time1	struct timeval        input timeval structure		*
 *									*
 * Output parameters:							*
 * lapse	long	time difference in milliseconds			*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	         8/94		animation			*
 ***********************************************************************/
{
long difft;

/*---------------------------------------------------------------------*/

           if (time0.tv_usec > time1.tv_usec) {

              time1.tv_usec += 1000000;
              time1.tv_sec--;

           }

	   difft = (long)((time1.tv_usec - time0.tv_usec)/1000. + 
			(time1.tv_sec - time0.tv_sec)*1000.);

	   return( difft ); /* in milliseconds */
}

