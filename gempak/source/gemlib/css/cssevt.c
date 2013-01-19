#include "geminc.h"
#include "gemprm.h"

/************************************************************************
 * cssevt.c								*
 *									*
 * This file contains the routines necessary to manage the event 	*
 * clock. 								*
 *									*
 * CONTENTS:								*
 *	css_evtsetinittm	- set the initial event time		* 
 *	css_evtgetinittm	- get the initial event time		*
 *	css_evtsetspeed		- set the event speed			*
 *	css_evtgetspeed		- get the event speed			*
 *	css_evtsetcurtm		- set the current event time		*
 *	css_evtpause		- set the event pause flag		*
 *	css_evtresume		- reset the event pause flag		*
 *	css_evtclear		- clear the event clock flag		*
 *	css_evtdumpvars		- print out values of global variables	*
 *	css_evtison		- return event clock status		*
 *	css_evtc2t 		- convert time string to time_t type	*
 **									*
 * Log:									*
 * B. Yin/SAIC   	03/04	Created                               	*
 * B. Yin/SAIC   	04/04	Changed event/system time to time_t type*
 * B. Yin/SAIC   	05/04	Added routine css_evtison		*
 ***********************************************************************/

/*
 * Global variables 
 */
 static time_t		_event_initial_time;
 static time_t		_event_current_time;
 static Boolean         _event_clock_on 	= FALSE;
 static Boolean         _event_clock_running 	= FALSE;
 static float           _event_event_rate	= 1.0;
 static float           _event_system_rate	= 1.0;
 static time_t		_event_system_time;

/*
 * Private functions
 */
static void css_evtc2t ( char *, time_t *, int * );

void css_evtsetinittm( char *evttime, int *iret )
/************************************************************************
 * css_evtsetinittm							*
 *									*
 * This function set the initial event time and save the system time.	*
 *									*
 * Input parameters:							*
 *	evttime		char	initial event time			*
 *									*
 * Output parameters:							*
 *	iret		int	return value  				*
 *				 0:	set initial time successfully	*
 *				-1:	null time string		*
 *				-2:	invalid time format		*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC   	03/04	Created         			*
 * B. Yin/SAIC   	04/04	Changed global time variables to time_t *
 *				Set current event time 			*
 ***********************************************************************/
{
   int		ier;
/*---------------------------------------------------------------------*/

   *iret = 0;
   
   if ( evttime == NULL ) {
      *iret = -1;
      return;
   }

   /*
    * set system time, initial event time and current event time
    */
   _event_system_time = time( (long*)0 );

   css_evtc2t ( evttime, &_event_initial_time, &ier );
   if ( ier != 0 ) {
      *iret = -2;
      return;
   }

   _event_current_time = _event_initial_time;

   _event_clock_on = TRUE;
   _event_clock_running = TRUE;
} 

void css_evtgetinittm( char *evttime, int *iret )
/************************************************************************
 * css_evtgetinittm                                                     *
 *                                                                      *
 * This function gets the initial event time.                           *
 *                                                                      *
 * Input parameters:                                                    *
 *       None                                                           *
 *                                                                      *
 * Output parameters:                                                   *
 *      evttime		char	initial event time.			*
 *      iret            int     return value                            *
 *                               0:     get initial time successfully   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 * B. Yin/SAIC   	04/04	Changed global time variables to time_t *
 ***********************************************************************/
{
   struct tm	*t_tm;
/*---------------------------------------------------------------------*/

   t_tm = gmtime ( &_event_initial_time );
   sprintf( evttime, "%02d%02d%02d/%02d%02d", (t_tm->tm_year+1900)%100,
            t_tm->tm_mon+1, t_tm->tm_mday, t_tm->tm_hour, t_tm->tm_min );

   *iret = 0;
}

void css_evtsetspeed( float *evtrate, float *sysrate, int *iret )
/************************************************************************
 * css_evtsetspeed                                                      *
 *                                                                      *
 * This function sets the speed of the event clock relative to the	* 
 * system clock.					                *
 *                                                                      *
 * Input parameters:                                                    *
 *      evtrate		float 	event clock rate                        *
 *      sysrate		float 	system clock rate                       *
 *                                                                      *
 * Output parameters:                                                   *
 *      iret            int     return value                            *
 *                               0:     set rates successfully          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 ***********************************************************************/
{
   _event_event_rate  = *evtrate;
   _event_system_rate = *sysrate;
   *iret = 0;
}

void css_evtgetspeed( float *evtrate, float *sysrate, int *iret )
/************************************************************************
 * css_evtgetspeed                                                      *
 *                                                                      *
 * This function gets the speed of the event clock relative to the      *
 * system clock.                                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      None                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      evtrate		float 	event clock rate                        *
 *      sysrate		float 	system clock rate                       *
 *      iret            int     return value                            *
 *                               0:     get initial time successfully   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 ***********************************************************************/
{
   *iret = 0;

   *evtrate = _event_event_rate;
   *sysrate = _event_system_rate;

   return;
}

void css_evtsetcurtm( char *ecurtm, int *iret )
/************************************************************************
 * css_evtsetcurtm                                                      *
 *                                                                      *
 * This function update the current event time				*
 *                                                                      *
 * Input parameters:                                                    *
 *      ecurtm		char	current event time                      *
 *                                                                      *
 * Output parameters:                                                   *
 *      iret            int     return value                            *
 *				 0:	set current time successfully	*
 *				-1:	null time string		*
 *				-2:	invalid time format		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 * B. Yin/SAIC   	04/04	Changed global time variables to time_t *
 ***********************************************************************/
{
   int		ier;
/*---------------------------------------------------------------------*/

   if ( ecurtm == NULL ) {
      *iret = -1;
      return;
   }

   css_evtc2t ( ecurtm, &_event_current_time, &ier );
   if ( ier != 0 ) {
      *iret = -2;
      return;
   }

   *iret = 0;
}

void css_evtpause( int *iret )
/************************************************************************
 * css_evtpause								*
 *									*
 * This function is to set the event pause/runing flag.			*
 *									*
 * Input parameters:                                                    *
 *      None                                                            *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 * B. Yin/SAIC          05/04   Set current event time when pause       *
 ***********************************************************************/
{
   int ier;
/*---------------------------------------------------------------------*/

   if ( _event_clock_running ) {
      css_evtadvtime ( &_event_current_time, &ier );
      _event_clock_running = FALSE;
   }
   *iret = 0;
}

void css_evtresume( int *iret )
/************************************************************************
 * css_evtresume							*
 *									*
 * This function is to clear the event pause/running flag.		*
 *									*
 * Input parameters:                                                    *
 *      None                                                            *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 * B. Yin/SAIC          05/04   Set system time when resume             *
 ***********************************************************************/
{
   if ( !_event_clock_running ) {
      _event_system_time = time( (long*)0 );
      _event_clock_running = TRUE;
   }
   *iret = 0;
}

void css_evtclear( int *iret )
/************************************************************************
 * css_evtclear								*
 *									*
 * This function is to clear the event clock flag.			*
 *									*
 * Input parameters:                                                    *
 *      None                                                            *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/04   Created                                 *
 * B. Yin/SAIC          05/04   Reset values of global variables        *
 ***********************************************************************/
{
   _event_clock_on 		= FALSE;
   _event_clock_running 	= FALSE;
   _event_event_rate		= 1.0;
   _event_system_rate		= 1.0;
   _event_initial_time		= 0;
   _event_current_time		= 0;
   _event_system_time		= 0;

   *iret = 0;
}

void css_evtdumpvars( void )
/************************************************************************
 * css_evtdumpvars							*
 *									*
 * This function is to print out values of global variables.		*
 *									*
 * Input parameters:                                                    *
 *      None                                                            *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          04/04   Created                                 *
 * B. Yin/SAIC   	04/04	Changed global time variables to time_t *
 ***********************************************************************/
{
   struct tm	*t_tm;
/*---------------------------------------------------------------------*/

   t_tm = gmtime ( &_event_initial_time );
   printf ( "\nEvent Initial Time: \t%02d%02d%02d/%02d%02d\n", 
		(t_tm->tm_year+1900)%100, t_tm->tm_mon+1, 
		 t_tm->tm_mday, t_tm->tm_hour, t_tm->tm_min );

   t_tm = gmtime ( &_event_current_time );
   printf ( "Event Current Time: \t%02d%02d%02d/%02d%02d\n", 
                (t_tm->tm_year+1900)%100, t_tm->tm_mon+1, 
                 t_tm->tm_mday, t_tm->tm_hour, t_tm->tm_min );

   printf ( "Event Clock On Flag: \t%s\n", 
	        ( _event_clock_on == 0 )? "False" : "True" );
   printf ( "Event Clock Running Flag: \t%s\n", 
		( _event_clock_running == 0 )? "False" : "True" );
   printf ( "Event Clock Rate: \t%f\n", _event_event_rate );
   printf ( "System Clock Rate: \t%f\n", _event_system_rate );

   t_tm = gmtime ( &_event_system_time );
   printf ( "Event System Time: \t%02d%02d%02d/%02d%02d\n", 
		(t_tm->tm_year+1900)%100, t_tm->tm_mon+1, 
                 t_tm->tm_mday, t_tm->tm_hour, t_tm->tm_min );
}


static void css_evtc2t ( char *time_str, time_t *t, int *iret )
/************************************************************************
 * css_evtc2t								*
 *									*
 * This function convert time from GEMPAK format to time_t 		*
 *									*
 * Input parameters:                                                    *
 *      *time_str	char		time string in GEMPAK format    *
 * Output parameters:                                                   *
 *      *t		time_t          time in time_t format           *
 *      *iret		int		0: normal return                *
 *      				-1: time_str not GEMPAK format  *
 * Return parameters:                                                   *
 *      None								*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          04/04   Created                                 *
 * B. Yin/SAIC          05/04   Modified to work on all platforms       *
 ***********************************************************************/
{
   int days_per_month[] =
  		{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
   int		idtarr[ 5 ], ier, i;
   long 	s_p_min, s_p_hour, s_p_day, s_p_year;
/*---------------------------------------------------------------------*/

   /*
    * check the time format valid or not
    */
   ti_ctoi( time_str, idtarr, &ier, strlen( time_str ) );
   if ( ier != 0 ) {
      *iret = -1;
      return;
   }

   /*
    * calculate seconds per known intervals 
    */
   s_p_min  = 60;
   s_p_hour = 60  * s_p_min;
   s_p_day  = 24  * s_p_hour;
   s_p_year = 365 * s_p_day;

   idtarr[0] -= 1970;
   
   *t  = idtarr[0] * s_p_year;			/* seconds per year since epoch */
   *t += (( idtarr[0] + 1 ) / 4) * s_p_day;	/* add in the leap years        */
   
   if ((( idtarr[0] + 2 ) % 4 ) == 0 ) {	/* check if this year is a leap */
      days_per_month[ 1 ]++;			/* and make feb one more day    */
   }

   /*
    * calculate seconds from january until the start of this month 
    */

   i = 0;
   idtarr[1]--;
   while ( i < idtarr[1] ) {
         *t += days_per_month[ i++ ] * s_p_day;
   }

   /*
    * calculate seconds of the days this month   
    */

   *t += --idtarr[ 2 ] * s_p_day;

   /*
    * calculate seconds of the hour this day 
    */

   *t += idtarr[ 3 ] * s_p_hour;

   /*
    * calculate seconds of the minutes this hour 
    */

   *t += idtarr[ 4 ] * s_p_min;

   *iret = 0;
}


void css_evtison( Boolean * clockon, int * iret )
/************************************************************************
 * css_evtison                                                          *
 *                                                                      *
 * This function returns the status of the event clock.                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      None                                                            *
 * Output parameters:                                                   *
 *      _event_clock_on         Boolean         event clock status      *
 *      *iret			int		0: normal return        *
 * Return parameters:                                                   *
 *      None                                                            *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          04/04   Created                                 *
 ***********************************************************************/
{
   *iret = 0;
   *clockon = _event_clock_on;
}


void css_evtadvtime ( time_t *evtelapse, int *iret )
/************************************************************************
 * css_evtadvtime                                                       *
 *                                                                      *
 * This function computes the elapsed time of the event clock           *
 *                                                                      *
 * Input parameters:                                                    *
 *      None                                                            *
 * Output parameters:                                                   *
 *      *evtelapse      time_t          elapsed time                    *
 *      *iret		int		0: normal return                *
 * Return parameters:                                                   *
 *      None                                                            *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          04/04   Created                                 *
 ***********************************************************************/
{
   time_t       sys_t;
/*---------------------------------------------------------------------*/

   if ( _event_clock_running ) {

      /*
       * get system time
       */
      sys_t = time( (long*)0 );

      *evtelapse = (long)(difftime( sys_t, _event_system_time ) *
                   (double)_event_event_rate / (double)_event_system_rate +
                   _event_current_time);
   }
   else {
      *evtelapse = _event_current_time;
   }

   *iret = 0;
}

