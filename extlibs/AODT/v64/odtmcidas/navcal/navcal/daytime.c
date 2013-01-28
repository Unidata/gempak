/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */


/**** $Id: daytime.c,v 1.1 2001/04/16 20:59:08 daves Exp $ ****/

#include <time.h>
#include <string.h>
#include <stdlib.h>
#include "mcidas.h"

static int
  M0basesecs       (time_t *);
static int
  M0tmtocydhms     (struct tm * , int *, int *);

/*
*$ Name:
*$      gettim - Gets the current system time in hhmmss format.
*$
*$ Interface:
*$      subroutine
*$      gettim (integer hms)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      hms   - Current time in hhmmss format.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      The C language version of this subroutine is Mcgettime().
*$
*$ Categories:
*$      day/time
*/

void gettim_ (Fint *hms)
{
  (void) Mcgettime ((int *) hms);
  return;
}

/*
*$ Name:
*$      Mcgettime - Gets the current system time in hhmmss format.
*$
*$ Interface:
*$      int
*$      Mcgettime (int *hms)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      hms       - Current time in hhmmss format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Unable to get current system time.
*$     -2         - Unable to convert to gmt.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mcgettime (int *hms)
{
  int ok;
  int day;

  ok = Mcgetdaytime (&day, hms);

  return (ok);
}

/*
*$ Name:
*$      Mcgetdaytime - Gets the current system day and time.
*$
*$ Interface:
*$      int
*$      Mcgetdaytime (*day , *hms)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day          - Current day in ccyyddd format.
*$      hms          - Current time in hhmmss format.
*$
*$ Return values:
*$      0            - Success.
*$     -1            - Unable to get current system time.
*$     -2            - Unable to convert current time to gmt.
*$     -3            - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      The time returned will be in UTC.
*$
*$ Categories:
*$      day/time
*/

int Mcgetdaytime (int *day , int *hms)
{
  struct tm             *t_tm;           /* temporary time structure */
  time_t                 current_time;   /* current time offset */
  int                    ok;             /* function return value */

  /* get the current system time */

  current_time = time ((time_t *) NULL);

  /* if you can't get the current time */

  if (current_time == (time_t) NULL)
  {
    return (-1);
  }

  /* convert the current time to the time structure for gmt */

  t_tm = gmtime (&current_time);
  if (t_tm == (struct tm *) NULL)
  {
    return (-2);
  }

  /* convert to ccyyddd and hhmmsss format */

  ok = M0tmtocydhms (t_tm , day , hms);
  if (ok < 0)
  {
    return (-3);
  }

  return (0);
}

/*
*$ Name:
*$      getday - Gets the current system day in yyddd format.
*$
*$ Interface:
*$      subroutine
*$      getday (integer day)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day   - Current day in yyddd format.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      This is FORTRAN-callable.
*$      The first two digits (cc) of the output field are removed.
*$
*$ Categories:
*$      day/time
*$
*/

void getday_ (Fint *day)
{
  (void) Mcgetday (day);
  *day = (Fint) ((*day) % 100000);

  return;
}

/*
*$ Name:
*$      Mcgetday - Gets the current system day in ccyyddd format.
*$
*$ Interface:
*$      int
*$      Mcgetday (*day)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day       - Current day in ccyyddd format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Unable to get current system day.
*$     -2         - Unable to convert current day to gmt.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*$
*/

int Mcgetday (int *day)
{
  int    ok;
  int    hms;

  ok = Mcgetdaytime (day, &hms);

  return (ok);
}

/*
*$ Name:
*$      mcgetday - Gets the current system day in ccyyddd format.
*$
*$ Interface:
*$      integer function
*$      mcgetday (integer day)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day    - Current day in ccyyddd format.
*$
*$ Return values:
*$      0      - Success.
*$     -1      - Unable to get current system day.
*$     -2      - Unable to convert current day to gmt.
*$
*$ Remarks:
*$      The C language version of this function is Mcgetday().
*$
*$ Categories:
*$      day/time
*/

Fint mcgetday_ (Fint *day)
{
  return ((Fint) Mcgetday ((Fint *) day));
}

/*
*$ Name:
*$      mcgetdaytime - Gets the current system day and time.
*$
*$ Interface:
*$      integer function
*$      mcgetdaytime (integer day , integer time)
*$
*$ Input:
*$      none
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day         - Current day in ccyyddd format.
*$      time        - Current time in hhmmss format.
*$
*$ Return values:
*$      0           - Success.
*$     -1           - Unable to get current system time.
*$     -2           - Unable to convert current time to gmt.
*$
*$ Remarks:
*$      The C language version of this function is Mcgetdaytime ().
*$
*$ Categories:
*$      day/time
*/

Fint mcgetdaytime_ (Fint *day , Fint *tim)
{
  int ok;

  ok =  Mcgetdaytime ((Fint *) day, (Fint *) tim);

  return ((Fint) ok);
}

/*
*$ Name:
*$      mcsectodaytime - Converts seconds since 1 January 1970 to
*$                       ccyyddd/hhmmss.
*$
*$ Interface:
*$      integer
*$      mcsectodaytime (integer secs , integer day, integer hms)
*$
*$ Input:
*$      secs    - Seconds since 1 January 1970.
*$
*$ Input and Output:
*$    none
*$
*$ Output:
*$      day     - Day to convert in ccyyddd format.
*$      hms     - Time to convert in hhmmss format.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - Unable to convert seconds to system structure.
*$     -2       - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      This is the FORTRAN callable version of Mcsectodaytime().
*$
*$ Categories:
*$      day/time
*/

Fint mcsectodaytime_ (Fint *secs , Fint *day, Fint *hms)
{
  int ok;

  ok = Mcsectodaytime ((time_t) *secs, (int *) day, (int *) hms);

  return ((Fint) ok);

}

/*
*$ Name:
*$      mcdaytimetosec - Converts day/time to the number of seconds since
*$                       00z 1 January, 1970.
*$
*$ Interface:
*$      integer
*$      mcdaytimetosec (integer day, integer hms, integer secs)
*$
*$ Input:
*$      day   - Day to convert in ccyyddd format.
*$      hms   - Time to convert in hhmmss format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      secs  - Number of seconds since 00Z 1 January 1970.
*$
*$ Return values:
*$      0     - Success.
*$     -1     - Unable to convert base time to system value.
*$     -2     - Unable to convert source time to system value.
*$     -3     - Invalid day format entered.
*$     -4     - Invalid time format entered.
*$
*$ Remarks:
*$      This is the FORTRAN callable version of Mcdaytimetosec().
*$
*$ Categories:
*$      day/time
*/

Fint mcdaytimetosec_ (Fint *day, Fint *hms, Fint *secs)
{
  int ok;

  ok = Mcdaytimetosec ((int) *day, (int) *hms, (time_t *) secs);

  return ((Fint) ok);

}

/*
*$ Name:
*$      Mcdaytimetosec - Converts the system's clock to absolute number of
*$                       seconds since 00z on 1 January 1970
*$
*$ Interface:
*$      include "time.h"
*$      int
*$      Mcdaytimetosec (int day, int hms, time_t *secs)
*$
*$ Input:
*$      day     - Day to convert in ccyyddd format.
*$      hms     - Time to convert in hhmmss format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      secs    - Number of seconds since 00Z 1 January 1970.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - Unable to convert base time to system value.
*$     -2       - Unable to convert source time to system value.
*$     -3       - Invalid day format.
*$     -4       - Invalid time format.
*$
*$ Remarks:
*$      The base time is 00:00:00 on 1 January 1970.
*$
*$      This function should work fine until approximately the year
*$      2099. At that time the number of seconds since 1 Jan 1970 will
*$      no longer fit in a 32 bit unsigned integer.
*$
*$ Categories:
*$      day/time
*/

int Mcdaytimetosec (int day, int hms, time_t *secs)
{

  struct tm       t = {0};           /* temporary time structure */
  time_t          tsecs;             /* temporary seconds since 1970 */
  time_t          basesecs;          /* base seconds of 1970 */
  int             ok;                /* function return value */
  int             day_7;             /* julian day in 7 digit format */

  ok = Mcydtocyd (day, &day_7);

  /* check for valid inputs */

  if (Mccydok (day_7) != 0)
  {
    return (-3);
  }

  if (Mchmsok (hms) != 0)
  {
    return (-4);
  }

  /* ok, if we have made it to here, the data should be good */

  t.tm_sec    = hms % 100;
  t.tm_min    = ((hms / 100) % 100);
  t.tm_hour   = hms / 10000;
  t.tm_mday   = (day_7 % 1000);
  t.tm_year   = (day_7 / 1000) - 1900;

  tsecs = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-2);
  }

  /* now create the base time seconds for 1 january 1970 */

  ok = M0basesecs (&basesecs);
  if (ok < 0)
  {
    return (-1);
  }

  *secs = tsecs - basesecs;

  return (0);
}

/*
*$ Name:
*$      Mcinctime - Increments a julian day and time by hhmmss increment.
*$
*$ Interface:
*$      int
*$      Mcinctime (int srcday , int srchms , int n_hms , int *destday,
*$                 int *desthms)
*$
*$ Input:
*$      srcday    - Julian day to be converted in ccyyddd format.
*$      srchms    - Hour of day to be converted in hhmmss format.
*$      n_hms     - Time increment in hhmmss format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      destday   - New Julian day.
*$      desthms   - New hour of day in hhmmss format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Invalid day entered.
*$     -2         - Invalid time entered.
*$     -3         - Unable to convert from cyd to seconds.
*$     -4         - Unable to convert from seconds to cyd.
*$     -5         - Invalid time increment entered.
*$
*$ Remarks:
*$      Set n_hms > 0 to increment forward in time. Set n_hms < 0 to
*$      decrement.
*$
*$ Categories:
*$      day/time
*$
*/

int Mcinctime (int srcday , int srchms , int n_hms , int *destday,
               int *desthms)
{
  static const int seconds_per_hour = 3600;
  static const int seconds_per_min  = 60;

  time_t           seconds;                  /* absolute seconds */
  int              hrs;                      /* hours to increment in n_hms */
  int              mins;                     /* minutes to increment */
  int              secs;                     /* seconds to increment */
  int              total_inc;                /* total number of seconds to
					      * increment */
  int              ok;                       /* function return value */
  int              day_7;                    /* julian day in 7 digit
                                              * format */

  ok = Mcydtocyd (srcday, &day_7);

  /* check for input error */

  ok = Mccydok (day_7);
  if (ok < 0)
  {
    return (-1);
  }
  ok = Mchmsok (srchms);
  if (ok < 0)
  {
    return (-2);
  }

  /*  check to see if increment is okay */
  ok = Mchmsok (abs (n_hms) % 10000);
  if (ok < 0)
  {
    return (-5);
  }

  /* convert day to seconds since 1 January 1970 */

  ok = Mcdaytimetosec (day_7 , srchms , &seconds);
  if (ok < 0)
  {
    return (-3);
  }

  /* increment appropriately */

  hrs        = (abs(n_hms) / 10000);
  total_inc  = hrs  * seconds_per_hour;

  mins       = ((abs (n_hms) / 100) % 100);
  total_inc += mins * seconds_per_min;

  secs       = (abs (n_hms) % 100);
  total_inc += secs;

  total_inc  = (n_hms >= 0) ? total_inc : (-1 * total_inc);

  seconds += total_inc;

  /* convert new number of seconds back to julian day */

  ok = Mcsectodaytime (seconds , destday , desthms);

  if (ok < 0)
  {
    return (-4);
  }

  return (0);

}

/*
*$ Name:
*$      mcinctime - Increments a julian day and time by hhmmss time
*$                increment.
*$
*$ Interface:
*$      integer function
*$      mcinctime (integer srcday , integer srchms , integer nhhmmss ,
*$                 integer dstday, integer dsthms)
*$
*$ Input:
*$      srcday    - Julian day in ccyyddd format to be converted.
*$      srchms    - Hour of day to be converted in hhmmss format.
*$      nhhmmss   - Number of hours to increment.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dstday    - New Julian day.
*$      dsthms    - New hour of day in hhmmss format.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Invalid day entered.
*$     -2         - Invalid time entered.
*$     -3         - Unable to convert from cyd to seconds.
*$     -4         - Unable to convert from seconds to cyd.
*$     -5         - Invalid time increment entered.
*$
*$ Remarks:
*$      Set nhhmmss > 0 to increment forward in time. Set nhhmmss < 0 to
*$      decrement.
*$
*$      This is the FORTRAN-callable version of Mcinctime().
*$
*$ Categories:
*$      day/time
*$
*/

Fint mcinctime_ (Fint *srcday , Fint *srchms , Fint *nhhmmss , Fint *dstday,
                 Fint *dsthms)
{
  int ok;

  ok = Mcinctime ((int) *srcday , (int) *srchms , (int) *nhhmmss ,
                  (int *) dstday , (int *) dsthms);
  return (ok);
}

/*
*$ Name:
*$      mcincday - Increments a julian day by a day count.
*$
*$ Interface:
*$      integer function
*$      mcincday (integer srcday , integer ndays , integer dstday)
*$
*$ Input:
*$      srcday   - Julian day in ccyyddd format to be converted.
*$      ndays    - Number of days to increment.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dstday   - New Julian day.
*$
*$ Return values:
*$      0        - Success.
*$     -1        - Invalid day entered.
*$     -2        - Unable to convert from cyd to seconds.
*$     -3        - Unable to convert from seconds to cyd.
*$
*$ Remarks:
*$      Set ndays > 0 to increment forward in time. Set ndays < 0 to
*$      decrement.
*$
*$      This is the FORTRAN-callable version of Mcincday().
*$
*$ Categories:
*$      day/time
*$
*/

Fint mcincday_ (Fint *srcday , Fint *ndays , Fint *dstday)
{
  int ok;

  ok = Mcincday ((int) *srcday , (int) *ndays , (int *) dstday);
  return (ok);
}

/*
*$ Name:
*$      Mcincday - Increments a julian day by a day count.
*$
*$ Interface:
*$      int
*$      Mcincday (int srcday , int n_days , int *destday)
*$
*$ Input:
*$      srcday   - Julian day in ccyyddd format to be converted.
*$      n_days   - Number of days to increment.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      destday  - New Julian day.
*$
*$ Return values:
*$      0        - Success.
*$     -1        - Invalid day entered.
*$     -2        - Unable to convert from cyd to seconds.
*$     -3        - Unable to convert from seconds to cyd.
*$
*$ Remarks:
*$      Set n_days > 0 to increment forward in time. Set n_days < 0 to
*$      decrement.
*$
*$ Categories:
*$      day/time
*$
*/

int Mcincday (int srcday , int n_days , int *destday)
{
  static int       seconds_per_day = 86400;
  time_t           seconds;                  /* absolute seconds */
  int              hms;                      /* time of day */
  int              ok;                       /* function return value */
  int              day_7;                    /* julian day in 7 digit
                                              * format */

  ok = Mcydtocyd (srcday, &day_7);

  /* check for input error */

  ok = Mccydok (day_7);
  if (ok < 0)
  {
    return (-1);
  }

  /* convert day to seconds since 1 January 1970 */

  hms = 0;
  ok = Mcdaytimetosec (day_7 , hms , &seconds);
  if (ok < 0)
  {
    return (-2);
  }

  /* increment appropriately */

  seconds += (seconds_per_day * n_days);

  /* convert new number of seconds back to julian day */

  ok = Mcsectodaytime (seconds , destday , &hms);

  if (ok < 0)
  {
    return (-3);
  }

  return (0);

}

/*
*$ Name:
*$      Mcdmytocyd - Converts day/month/year to ccyyddd.
*$
*$ Interface:
*$      int
*$      Mcdmytocyd (int day, int month, int year, int *ccyyddd)
*$
*$ Input:
*$      day        - Day of month
*$      month      - Month of year (1 = january)
*$      year       - Year
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd    - Julian day
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Year is out of range, 1970 - 2100.
*$     -2          - Month doesn't make sense, range (1-12).
*$     -3          - Day is not consistent with month and year.
*$     -4          - Unable to convert entered day to system time.
*$     -5          - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      The range of years must be between 1970 and 2100.
*$      The month range is 1 to 12.
*$
*$      Valid day numbers will vary with the month and year.
*$
*$ Categories:
*$      day/time
*/

int Mcdmytocyd (int day, int month, int year, int *ccyyddd)
{
  static int    days_in_month[] =
                                 {31,28,31,30,31,30,31,31,30,31,30,31};
  struct tm     t = {0};         /* structure to build mktime request */
  time_t        tsecs;           /* system time from mktime */
  int           ok;              /* function return value */
  int           hms;             /* time in hhmmss */

  /* check inputs */

  if (year < 1970 || year > 2100)
  {
    return (-1);
  }
  if (month < 1 || month > 12)
  {
    return (-2);
  }

  /* if a leap year, february has 29 days, otherwise it has 28 days */

  days_in_month[1] = (year % 4) ? 28 : 29;

  /* make certain the day makes sense for the given month */

  if (day < 1 || (day > days_in_month[month-1]))
  {
    return (-3);
  }

  /* build the time structure and get the system time */

  t.tm_year  = year - 1900;
  t.tm_mon   = month - 1;
  t.tm_mday  = day;

  tsecs      = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-4);
  }

  ok = M0tmtocydhms (&t , ccyyddd , &hms);
  if (ok < 0)
  {
    return (-5);
  }

  return (0);
}

/*
*$ Name:
*$      mcdmytocyd - Converts day/month/year to ccyyddd.
*$
*$ Interface:
*$      integer function
*$      mcdmytocyd (integer day, integer month, integer year,
*$                  integer ccyyddd)
*$
*$ Input:
*$      day       - Day of month.
*$      month     - Month of year (1 = january).
*$      year      - Year.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd   - Julian day.
*$
*$ Return values:
*$      0         - Success.
*$     -1         - Year is out of range, 1970 - 2100.
*$     -2         - Month doesn't make sense, range (1-12).
*$     -3         - Day is not consistent with month and year.
*$     -4         - Unable to convert entered day to system time.
*$
*$ Remarks:
*$      This is the FORTRAN-callable version of Mcdmytocyd().
*$
*$ Categories:
*$      day/time
*/

int mcdmytocyd_ (Fint *day, Fint *month, Fint *year, Fint *ccyyddd)
{
  int ok;

  ok = Mcdmytocyd ((int) *day, (int) *month, (int) *year, (int *) ccyyddd);

  return (ok);
}

/*
*$ Name:
*$      Mccydtodmy - Converts ccyyddd to day/month/year.
*$
*$ Interface:
*$      int
*$      Mccydtodmy (int ccyyddd, int *day, int *month, int *year)
*$
*$ Input:
*$      ccyyddd    - Julian day in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day        - Day of the month  (range 1 - 31).
*$      month      - Month of the year (range 1 - 12).
*$      year       - Year (range 1970 - 2100).
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Invalid input format.
*$     -2          - General error.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mccydtodmy (int ccyyddd , int *day, int *mon, int *year)
{
  time_t        tsecs;           /* absolute system seconds */
  struct tm     t = {0};         /* structure to build mktime request */
  int           day_7;           /* day in 7 digit format */
  int           ok;

  ok = Mcydtocyd (ccyyddd, &day_7);

  if (Mccydok (day_7) != 0)
  {
    return (-1);
  }

  /* ok, if we have made it to here, the data should be good */

  t.tm_mday   = (day_7 % 1000);
  t.tm_year   = (day_7 / 1000) - 1900;

  tsecs = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-2);
  }

  *day  = t.tm_mday;
  *mon  = t.tm_mon + 1;
  *year = 1900 + t.tm_year;

  return (0);
}

/*
*$ Name:
*$      mccydtodmy - Converts ccyyddd to day/month/year.
*$
*$ Interface:
*$      integer function
*$      mccydtodmy (integer ccyyddd, integer day, integer month,
*$                  integer year)
*$
*$ Input:
*$      ccyyddd    - Julian day in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      day        - Day of the month  (range 1 - 31).
*$      month      - Month of the year (range 1 - 12).
*$      year       - Year (range 1970 - 2100).
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Invalid input format.
*$     -2          - General error.
*$
*$ Remarks:
*$      This is the FORTRAN-callable version of Mccydtodmy().
*$
*$ Categories:
*$      day/time
*/

int mccydtodmy_ (Fint *ccyyddd , Fint *day, Fint *mon, Fint *year)
{
  int ok;

  ok = Mccydtodmy ((int) *ccyyddd, (int *) day, (int *) mon,
                   (int *) year);

  return (ok);

}

/*
*$ Name:
*$      Mcsectodaytime - Converts number of seconds since 1 January 1970 to
*$                       day/time.
*$
*$ Interface:
*$      int
*$      Mcsectodaytime (time_t secs, int *cyd, int *hms)
*$
*$ Input:
*$      secs           - Seconds since 1 January 1970.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      cyd           - Julian date in ccyyddd format.
*$      hms           - Time in hhmmss format.
*$
*$ Return values:
*$      0             - Success.
*$     -1             - Unable to convert seconds to system structure.
*$     -2             - Unable to convert to ccyyddd and hhmmss.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mcsectodaytime (time_t secs, int *cyd, int *hms)
{
  struct tm             *t_tm;           /* temporary time structure */
  int                    ok;             /* function return value */

  /* convert the seconds to the tm structure */

  t_tm = gmtime (&secs);
  if (t_tm == (struct tm *) NULL)
  {
    return (-1);
  }

  /* convert the tm structure to ccyyddd and hhmmss */

  ok = M0tmtocydhms (t_tm , cyd , hms);
  if (ok < 0)
  {
    return (-2);
  }

  return (0);
}

/*
*| Name:
*|      M0basesecs - Gets the system's base seconds at 00:00:00UTC
*|                 on 1 January 1970
*| Interface:
*|      include <time.h>
*|      int
*|      M0basesecs (time_t *secs)
*|
*| Input:
*|      none
*|
*| Input and Output
*|      none
*|
*| Output:
*|      secs        - System seconds at 00:00:00UTC on 1 January 1970.
*|
*| Return values:
*|      0           - Success.
*|     -1           - Unable to calculate the base time.
*|
*| Remarks:
*|      none
*|
*| Categories:
*|      day/time
*/

static int M0basesecs(time_t *secs)
{
  static time_t          seconds = 0;       /* base number of seconds */

  /*
   * if the base seconds have not previously been acquired, or there
   * was an error, get the base seconds
   */

  if (seconds == 0 || seconds == -1)
  {
    struct tm       bt = {0};               /* base time structure */

    (void) memset (&bt, 0, sizeof (struct tm));

   /* build the structure for 00:00:00 on 1 January 1970 */

    /*
     * note:
     * in timezones east of Grenwitch,
     * jan 1 1970 is a negative number
     * which mktime doesn't like
     * so we ask for Jan 2nd instead, and subrtact the number
     * of seconds in a day
     */

    bt.tm_year  = 70;
    bt.tm_mday  = 2;

    seconds = mktime (&bt)-86400;

    /* if there was a error, return and try again later */

    if (seconds == (time_t) -1)
    {
      *secs = seconds;
      return (-1);
    }
  }

  *secs = seconds;

  return (0);
}

/*
*| Name:
*|      M0tmtocydhms - Converts the C time structure,tm , to ccyyddd and
*|                   hhmmss format.
*|
*| Interface:
*|      include <time.h>
*|      int M0tmtocydhms (struct tm *time_struct, int *cyd, int *hms)
*|
*| Input:
*|      time_struct  - Time structure to be converted.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      cyd          - Julian day in ccyyddd format.
*|      hms          - Time in hhmmsss format.
*|
*| Return values:
*|      0            - Success.
*|     -1            - Unable to convert.
*|
*| Remarks:
*|      For a complete listing of the time structure see page 345 in the
*|      book "C a Reference Manual" third edition by Harbison and Steele.
*|
*| Categories:
*|      day/time
*|
*/

static int M0tmtocydhms (struct tm *time_str, int *cyd, int *hms)
{

  /* convert the julian day */

  *cyd   = (1900 + time_str->tm_year) * 1000 +
           (time_str->tm_yday + 1);

  /* convert the necessary portions of the time structure to hhmmss */

  *hms   = (time_str->tm_hour * 10000) +
           (time_str->tm_min  * 100) +
            time_str->tm_sec;

  return (0);
}

/*
*$ Name:
*$      Mccydok - Verifies that the ccyyddd format is correct.
*$
*$ Interface:
*$      int
*$      Mccydok (int ccyyddd)
*$
*$ Input:
*$      ccyyddd - Julian day to test.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$     -3       - (ccyyddd < 1900000) or (ccyyddd > 9999365).
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mccydok (int ccyyddd)
{
  int             jday;              /* input julian day */
  int             year;              /* input year */
  int             leap;              /* leap year flag */

  /*
   * Basic check for dates before 1900001 and dates greater
   * than 9999365. 
   */

  if (ccyyddd < 1900001 || ccyyddd > 9999365)
  {
    return (-3);
  }

  year    = ccyyddd / 1000;
  leap    = (year % 4) ? 0 : 1;
  jday    = ccyyddd % 1000;

  /*
   * if the julian day is greater that 366 or greater than 365 and
   * it is not a leap year, or the julian day is 0
   */

  if ((jday > 366) || (jday == 366 && leap == 0) || jday == 0)
  {
    return (-2);
  }

  /*
   * For applications requiring pre-1970 check:
   * The valid day should be between 1 jan 1970 and about
   * the year 2100.  You can't store enough seconds in a 32 bit
   * unsigned integer to go beyond about 130 years.
   */

  if (ccyyddd < 1970001 || ccyyddd > 2100001)
  {
    return (-1);
  }


  return (0);
}

/*
*$ Name:
*$      mccydok - Verifies that the ccyyddd format is correct.
*$
*$ Interface:
*$      integer function
*$      mccydok (integer ccyyddd)
*$
*$ Input:
*$      ccyyddd - Julian day to test.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$     -3       - (ccyyddd < 1900000) or (ccyyddd > 9999365).
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

Fint
  mccydok_ (Fint *ccyyddd)
{
  return ((Fint) Mccydok ((int) *ccyyddd));
}

/*
*$ Name:
*$      Mchmsok - Checks format of the input field (HHMMSS).
*$
*$ Interface:
*$      int
*$      Mchmsok (int hhmmss)
*$
*$ Input:
*$      hhmmss  - Field whose format is being tested.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -3       - minute > 59.
*$     -4       - second > 59.
*$
*$ Remarks:
*$      This function just checks to be certain that neither the
*$      minute nor the second count is greater that 59.
*$
*$      The input time can be positive or negative. If you want
*$      to specifically check for a valid time of day call McIsTimeOfDay().
*$
*$ Categories:
*$      day/time
*/

int Mchmsok (int hms)
{

  int        abshms;       /* absolute value of entered hms */
  int        minute;       /* minute of the hour */
  int        second;       /* second of the minute */

  abshms = abs (hms);

  /* do specific checks for time */

  minute  = ((abshms / 100) % 100);
  second  = abshms % 100;

  /* check to make certain the minute and second is ok */

  if (minute > 59)
  {
    return (-3);
  }
  if (second > 59)
  {
    return (-4);
  }
  return (0);
}

/*
*$ Name:
*$      mchmsok - Checks the format of the input field (HHMMSS).
*$
*$ Interface:
*$      integer function
*$      mchmsok (integer hhmmss)
*$
*$ Input:
*$      hhmmss  - Field being tested.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0       - Success.
*$     -3       - minute > 59.
*$     -4       - second > 59.
*$
*$ Remarks:
*$      This function just checks to be certain that neither the
*$      minute nor the second count is greater that 59.
*$
*$      The input time can be positive or negative. If you want
*$      to specifically check for a valid time of day call
*$      mcistimeofday().
*$
*$ Categories:
*$      day/time
*/

Fint
  mchmsok_ (Fint *hhmmss)
{
  return ((Fint) Mchmsok ((int) *hhmmss));
}

/*
*$ Name:
*$      McIsTimeOfDay - Determines whether a time value is actually
*$                      a time of day.
*$
*$ Interface:
*$      int
*$      McIsTimeOfDay (int hms)
*$
*$ Input:
*$      hms       - time value to test
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0         - Entered value is a time of day.
*$     <0         - Entered value is not a time of day.
*$
*$ Remarks:
*$      This function checks to make certain that the entered time
*$      is greater or equal to 0 and less than or equal to 235959. It
*$      also verifies that both the minutes and seconds portions are
*$      less than 60.
*$
*$ Categories:
*$      day/time
*/

int
  McIsTimeOfDay (int hms)
{
  if (hms < 0 || hms > 235959)
  {
    return (-1);
  }

  return (Mchmsok (hms));
}

/*
*$ Name:
*$      mcistimeofday -Determines whether a time value is actually
*$                      a time of day.
*$
*$ Interface:
*$      integer function
*$      mcistimeofday (integer hms)
*$
*$ Input:
*$      hms       - time value to test
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      0         - entered value is a time of day
*$     <0         - entered value is not a time of day
*$
*$ Remarks:
*$      This function checks to make certain that the entered time
*$      is greater or equal to 0 and less than or equal to 235959. It
*$      also verifies that both the minutes and seconds portions are
*$      less than 60.
*$
*$ Categories:
*$      day/time
*/

Fint
  mcistimeofday_ (Fint *hms)
{
  return ((Fint) McIsTimeOfDay ((int) *hms));
}

/*
*$ Name:
*$      Mccydtoyd - Converts a Julian day of the form ccyyddd to a Julian
*$                  day of the form yyddd.
*$
*$ Interface:
*$      int
*$      Mccydtoyd (int ccyyddd, int *yyddd)
*$
*$ Input:
*$      ccyyddd       - Source Julian day of the form ccyyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yyddd         - Resulting day of the form yydddc.
*$
*$ Return values:
*$      0             - Success.
*$     -1             - Failure.
*$
*$ Remarks:
*$      This function just performs a modulus of the source day.
*$
*$      If the source day is already in yyddd format, it will just
*$      move the same value into the destination day.
*$
*$ Categories:
*$      day/time
*/

int
  Mccydtoyd (int ccyyddd, int *yyddd)
{

  if (ccyyddd <= 0)
  {
    return (-1);
  }

  *yyddd = (ccyyddd % 100000);
  return (0);
}

/*
*$ Name:
*$      mccydtoyd - Converts a Julian day of the form ccyyddd to a Julian
*$                  day of the form yyddd.
*$
*$ Interface:
*$      integer function
*$      mccydtoyd (integer ccyyddd, integer yyddd)
*$
*$ Input:
*$      ccyyddd       - Source Julian day of the form ccyyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yyddd         - Resulting day of the form yyddd.
*$
*$ Return values:
*$      0             - Success.
*$     -1             - Failure.
*$
*$ Remarks:
*$      This function just performs a modulus of the source day.
*$
*$      If the source day is already in yyddd format, it will just
*$      move the same value into the destination day.
*$
*$ Categories:
*$      day/time
*/

Fint
  mccydtoyd_ (Fint *src, Fint *des)
{
  return ((Fint) Mccydtoyd ((int) *src, (int *) des));
}

/*
*$ Name:
*$      Mcydtocyd - Converts a Julian day of the form yyddd to a Julian
*$                  day of the form ccyyddd.
*$
*$ Interface:
*$      int
*$      Mcydtocyd (int yyddd, int *ccyyddd)
*$
*$ Input:
*$      yyddd      - Source Julian day of the form yyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd    - Resulting day of the form ccyyddd.
*$
*$ Return values:
*$      0          - Success.
*$     <0          - Failure.
*$
*$ Remarks:
*$
*$      If the source day is already in ccyyddd format, it will 
*$      just move the same value into the destination day.
*$
*$      If the year portion of yyddd is between 70 and 99, inclusive,
*$      this function will assume that the century you want is the 1900's.
*$
*$      If the year portion of yyddd is less than 70 this function will
*$      assume that the century you want is in the 2000s.
*$
*$ Categories:
*$      day/time
*/

int
  Mcydtocyd (int yyddd, int *ccyyddd)
{
  int      yd;                    /* guaranteed yyddd form */

  if (yyddd <= 0)
  {
    return (-1);
  }

  *ccyyddd = yyddd;

  yd = (yyddd % 100000);

  /* if yyddd is <= yd, assume it src was in yyddd form */

  if (yyddd <= yd)
  {
    int      year;                  /* year of century */

    year = yd / 1000;

    if (year < 70)
    {
      *ccyyddd = 2000000 + yd;
    }
    else
    {
      *ccyyddd = 1900000 + yd;
    }
  }
  return (0);
}

/*
*$ Name:
*$      mcydtocyd - Converts a Julian day of the form yyddd to a Julian
*$                  day of the form ccyyddd.
*$
*$ Interface:
*$      integer
*$      mcydtocyd (integer yyddd, integer ccyyddd)
*$
*$ Input:
*$      yyddd      - Source Julian day of the form yyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd    - Resulting day of the form ccyyddd.
*$
*$ Return values:
*$      0          - Success.
*$     <0          - Failure.
*$
*$ Remarks:
*$
*$      If the source day is already in ccyyddd format, it will
*$      just move the same value into the destination day.
*$
*$      If the year portion of yyddd is between 70 and 99, inclusive,
*$      this function will assume that the century you want is the 1900's.
*$
*$      If the year portion of yyddd is less than 70 this function will
*$      assume that the century you want is in the 2000s.
*$
*$ Categories:
*$      day/time
*/

Fint
  mcydtocyd_ (Fint *src, Fint *des)
{
  return ((Fint) Mcydtocyd ((int) *src, (int *) des));
}

/*
*$ Name:
*$      Mccydtodow - Converts ccyyddd to the day of the week
*$
*$ Interface:
*$      int
*$      Mccydtodow (int ccyyddd, int *dow);
*$
*$ Input:
*$      ccyyddd    - Julian day in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dow        - day of the week (0 = Sunday)
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Invalid input format.
*$     -2          - General error.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

int Mccydtodow (int ccyyddd , int *dow)
{
  time_t        tsecs;           /* absolute system seconds */
  struct tm     t = {0};         /* structure to build mktime request */
  int           day_7;           /* day in 7 digit format */
  int           ok;

  ok = Mcydtocyd (ccyyddd, &day_7);

  if (Mccydok (day_7) != 0)
  {
    return (-1);
  }

  /* ok, if we have made it to here, the data should be good */

  t.tm_mday   = (day_7 % 1000);
  t.tm_year   = (day_7 / 1000) - 1900;

  tsecs = mktime (&t);
  if (tsecs == (time_t) -1)
  {
    return (-2);
  }

  *dow  = t.tm_wday;

  return (0);
}

/*
*$ Name:
*$      mccydtodow - Converts ccyyddd to the day of the week
*$
*$ Interface:
*$      integer function
*$      mccydtodow (integer ccyyddd, integer dow)
*$
*$ Input:
*$      ccyyddd    - Julian day in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      dow        - day of the week (0 = Sunday)
*$
*$ Return values:
*$      0          - Success.
*$     -1          - Invalid input format.
*$     -2          - General error.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      day/time
*/

Fint mccydtodow_ (Fint *ccyyddd , Fint *dow)
{
  return Mccydtodow ((int) *ccyyddd, (int *) dow);
}

/*
*$ Name:
*$      Mciydtocyd - Converts yyyddd date format to ccyyddd.
*$
*$ Interface:
*$      int
*$      Mciydtocyd (int yyyddd, int *ccyyddd)
*$
*$ Input:
*$      yyyddd        - Date in yyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd       - Date in ccyyddd format.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$     -3       - (ccyyddd < 1900000) or (ccyyddd > 9999365).
*$
*$ Remarks:
*$      Input date format is yyyddd where ccyy=yyy+1900
*$      No error checking is done on the format of the input yyyddd.
*$
*$ Categories:
*$      day/time
*/


int Mciydtocyd (int yyyddd, int *ccyyddd)
{
       int day_ok;     /*  return code from Mccydok  */

  *ccyyddd = (yyyddd / 1000 + 1900) * 1000 + (yyyddd % 1000);

  day_ok=Mccydok(*ccyyddd);

  return (day_ok);
}

/*
*$ Name:
*$      mciydtocyd - Converts yyyddd date format to ccyyddd.
*$
*$ Interface:
*$      integer function
*$      mciydtocyd (integer yyyddd, integer ccyyddd)
*$
*$ Input:
*$      yyyddd        - Julian day as yyyddd.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ccyyddd       - Day as ccyyddd.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$     -3       - (ccyyddd < 1900000) or (ccyyddd > 9999365).
*$
*$ Remarks:
*$      The input date format is  yyyddd where ccyy=yyy+1900.
*$      No error checking is done on the format of the input yyyddd.
*$
*$ Categories:
*$      day/time
*/

Fint
  mciydtocyd_ (Fint *src, Fint *des)
{
  return ((Fint) Mciydtocyd ((int) *src, (int *) des));
}

/*
*$ Name:
*$      Mccydtoiyd - Converts ccyyddd day format to yyyddd.
*$
*$ Interface:
*$      int
*$      Mccydtoiyd (int ccyyddd, int *yyyddd)
*$
*$ Input:
*$      ccyyddd       - Date in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yyyddd        - Date in yyyddd format.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$     -3       - (ccyyddd < 1900000) or (ccyyddd > 9999365).
*$
*$ Remarks:
*$      Output date format is yyyddd where yyy is ccyy-1900.
*$
*$ Categories:
*$      day/time
*/

int Mccydtoiyd (int ccyyddd, int *yyyddd)
{
       int day_ok;    /*  Return code from Mccydok  */

  day_ok=Mccydok(ccyyddd);

  *yyyddd = (ccyyddd / 1000 - 1900) * 1000 + (ccyyddd % 1000);

  return (day_ok);
}

/*
*$ Name:
*$      mccydtoiyd - Converts ccyyddd day format to yyddd.
*$
*$ Interface:
*$      int
*$      mccydtoiyd (integer ccyyddd, integer yyyddd)
*$
*$ Input:
*$      ccyyddd       - Date in ccyyddd format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yyyddd        - Date in yyyddd format.
*$
*$ Return values:
*$      0       - Success.
*$     -1       - (ccyyddd < 1970000) or (ccyyddd > 2100000).
*$     -2       - The day portion > 365 or 366 for leap years.
*$     -3       - (ccyyddd < 1900000) or (ccyyddd > 9999365).
*$
*$ Remarks:
*$      The output format is yyyddd where yyy=ccyy-1900.
*$
*$ Categories:
*$      day/time
*/

Fint
  mccydtoiyd_ (Fint *src, Fint *des)
{
  return ((Fint) Mccydtoiyd ((int) *src, (int *) des));
}
