#include <dccmn.h>
#include <gbcmn.h>

#include "dcgrib.h"

/* Initial bulletin buffer size */
size_t MAXGRIBSIZE = 1000000;


/*
** Define and initialize the finite-state machine for finding a
** bulletin in the buffer.
*/

/* States for finding a bulletin */
typedef enum {
    GRIB_START,
    GRIB_NOT_IN_BULLETIN,
    GRIB_G_,
    GRIB_G_R_,
    GRIB_G_R_I_,
    GRIB_IN_BULLETIN,
    GRIB_7_,
    GRIB_7_7_,
    GRIB_7_7_7_,
    GRIB_COMPLETE
} fstategrib_t;

/* sec0 array must have entries for through VERSION_HIGH */
#define	VERSION_HIGH	2
const int sec0[]={8, 8, 16};

static fstategrib_t finite_state = GRIB_START;


unsigned char *dc_ggrib ( size_t *griblen, int *version, int *iret )
/************************************************************************
 * dc_ggrib								*
 *									*
 * This routine will check the buffer for GRIB data and return a 	*
 * pointer to the start of the GRIB bulletin as well as the length of	*
 * product. 		 						*
 *									*
 * *gribbul = dc_ggrib ( griblen, version, iret )			*
 *									*
 * Output parameters:							*
 *	*gribbul	unsigned char	Pointer to start of GRIB	*
 *	*griblen	size_t		length of GRIB message		*
 *	*version	int		GRIB version number		*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					 -50 = griblen too large	*
 *					 -51 = grib version not expected**
 *									*
 **									*
 * Log:									*
 * Chiz/Unidata		11/99		Created from dcgbul.c		*
 ***********************************************************************/

{

	static unsigned char	cret, ch;
	char bhead[20];
	int			ier,iret1;
	char errstr[80];
	static char errgrp[]="DCGRIB";
	int loglev,numerr;

	int i,bpos=0;

	static unsigned char *gribbul = NULL;

/*---------------------------------------------------------------------*/

	if (gribbul == NULL)
	    gribbul = (unsigned char *) malloc (MAXGRIBSIZE);

	*version = -1;
	*iret = 0;
	*griblen = 0;

/*
**	Loop forever. The loop will be broken if a complete
**	bulletin is found.
*/
	while ( 1 ) {

/*
**	    Check for an empty buffer.
*/
	    if  ( ( finite_state != GRIB_START ) ||
		  ( dcb_isempt() == 1 ) ) {


/*
**		Read from standard input to the buffer.
*/
		if  ( ( ier = dcb_put ( STDIN_FILENO, itmout ) ) < 0 ) {

/*
**		    Timeout or end of data file has occurred.
*/
                    loglev = 1; numerr = ier;
		    switch(numerr)
                       {
		       case -6:
				sprintf(errstr,"Time Out\0");
				break;
		       case -7:
				sprintf(errstr,"Buffer Full\0");
				break;
		       case -9:
				sprintf(errstr,"End of Data\0");
				break;
		       default:
				sprintf(errstr,"Buffer Error\0");
                       }
                    dc_wclg(loglev,errgrp,numerr,errstr,&iret1);
		    *iret = ier;
		    return(NULL);
		}

	    }

/*
**	    Scan the input buffer.
*/
	    while ( ( cret = dcb_peekc ( &ch ) ) != EOD ) {
		if(bpos > (MAXGRIBSIZE - 1)) /* did not find 7777 within bulletin */
		    {
                    loglev = 0; numerr = -60;
                    sprintf(errstr, "7777 not found at griblen [%d] [state %d]\0",
                                      *griblen,finite_state);
                    dc_wclg(loglev,errgrp,numerr,errstr,&ier);
		    finite_state = GRIB_NOT_IN_BULLETIN;
		    return(NULL);
		    }
		switch ( finite_state ) {

		    case GRIB_START:
			if  ( ch == 'G' ) {
			    finite_state = GRIB_G_;
			}
			else {
			    finite_state = GRIB_NOT_IN_BULLETIN;
			    dcb_sbhd (0);
			}
			break;

/*
**		    If not inside a bulletin, look for GRIB
*/
		    case GRIB_NOT_IN_BULLETIN:
			if  ( ch == 'G' ) {
			    finite_state = GRIB_G_;
			}
			else {
			    finite_state = GRIB_NOT_IN_BULLETIN;
			    dcb_sbhd (0); /* don't let buffer overflow
					     while looking for start */
			}
			break;

		    case GRIB_G_:
			if  ( ch == 'R' ) {
			    finite_state = GRIB_G_R_;
			}
			else
			    {
			        finite_state = GRIB_NOT_IN_BULLETIN;
			}
			break;

		    case GRIB_G_R_:
			if  ( ch == 'I' ) {
			    finite_state = GRIB_G_R_I_;
			}
			else
			    {
			        finite_state = GRIB_NOT_IN_BULLETIN;
			    }
			break;

		    case GRIB_G_R_I_:
			if  ( ch == 'B' ) {
			    finite_state = GRIB_IN_BULLETIN;
			    dcb_sbhd ( -3 );
                            gribbul[0] = 'G';
                            gribbul[1] = 'R';
                            gribbul[2] = 'I';
                            gribbul[3] = 'B';
			    bpos=4;
			}
			else
			    {
			        finite_state = GRIB_NOT_IN_BULLETIN;
			    }
			break;

		    case GRIB_IN_BULLETIN:
		        gribbul[bpos] = ch; bpos++;
			if(bpos == 8) *version = (int)gribbul[7];

			if ( *version > VERSION_HIGH )
			   {
			   printf("found GRIB version %d, maximum expected %d\n",
				*version, VERSION_HIGH );
		           *iret = -51;
			   return(NULL);
			   }

			if ( ( *version != -1 ) && ( bpos == sec0[*version] ) )
                           {
			   dcb_sbtl ( 0 );
                           ier = dcb_getb(bhead); /* ier should be less than sizeof(bhead) */

                           if(*version < 2)
			      *griblen = (size_t)gb_btoi((unsigned char *)bhead,4,3,0);
			   else
			      {
			      *griblen = (size_t)gb_btoi((unsigned char *)bhead,12,4,0);
			      *griblen = (size_t)(((unsigned char)bhead[12] << 24) +
				   ((unsigned char)bhead[13] << 16) +
				   ((unsigned char)bhead[14] << 8) +
				   (unsigned char)bhead[15]);
			      }

			   if(*griblen > MAXGRIBSIZE)
                              {
			      /*
           		       ** See if we can increase our working buffer size
			       ** NB: realloc will copy over the existing data in the buffer!
           		       */
			      unsigned char *ptr;
			      ptr = (unsigned char *) realloc (gribbul, *griblen);
			      if (ptr != NULL)
				 {
				 gribbul = ptr;
				 MAXGRIBSIZE = *griblen;

				 loglev = 0; numerr = 0;
				 sprintf (errstr, "Increased MAXGRIBSIZE to %d\0", MAXGRIBSIZE);
                                 dc_wclg (loglev, errgrp, numerr, errstr, &ier);
                                 }
                              else
			         {
			         finite_state = GRIB_NOT_IN_BULLETIN;
                                 loglev = 0; numerr = -50;
                                 sprintf(errstr, "griblen [%d] > maxgribsize [%d]\0",
                                         *griblen,(int)MAXGRIBSIZE);
                                 dc_wclg(loglev,errgrp,numerr,errstr,&ier);
				 if (gribbul != NULL) free(gribbul);
			         gribbul = (unsigned char *) malloc (MAXGRIBSIZE);
				 *iret = numerr;
			         return(NULL);
				 }
                              }
                           }

                        /* don't let buffer overflow, check for "7777" after length is found */
			if ( ( *version != -1 ) && (bpos > sec0[*version] ) ) 
                           {
                           dcb_sbhd (0);
                           if  ( ch == '7' )
                               finite_state = GRIB_7_;
			   if  ( bpos > *griblen )
			      {
                              loglev = 0; numerr = -60;
                              sprintf(errstr, "no 7777 found: bpos [%d] > griblength [%d]\0",
                                         bpos,*griblen);
                              dc_wclg(loglev,errgrp,numerr,errstr,&ier);
			      finite_state = GRIB_NOT_IN_BULLETIN; bpos = 0;
			      }
                           }
			break;

		    case GRIB_7_:
		        gribbul[bpos] = ch; bpos++;
			if  ( ch == '7' ) 
			    finite_state = GRIB_7_7_;
			else
			    finite_state = GRIB_IN_BULLETIN;
			dcb_sbhd (0);
			break;

		    case GRIB_7_7_:
		        gribbul[bpos] = ch; bpos++;
			if  ( ch == '7' ) 
			    finite_state = GRIB_7_7_7_;
			else
			    finite_state = GRIB_IN_BULLETIN;
			dcb_sbhd (0);
			break;

		    case GRIB_7_7_7_:
		        gribbul[bpos] = ch; bpos++;
			dcb_sbhd (0);
			if( ch == '7' ) {
                            if(bpos >= *griblen)
                               {
                               if(bpos > *griblen)
                                  {
                                  loglev = 0; numerr = -50;
                                  sprintf(errstr, "bulletin too long %d > %d\n",
				          bpos,*griblen);
                                  dc_wclg(loglev,errgrp,numerr,errstr,&ier);
				  finite_state = GRIB_NOT_IN_BULLETIN;
				  for ( i = 0; i < 16; i++)
				     printf ( "%d %u\n",i,bhead[i]);
				  *iret = -50;
                                  }
                               finite_state = GRIB_START;
			       return(gribbul);
                               }
			    else
			       {
			       loglev = 3; numerr = -77;
			       sprintf(errstr, "found 7777 in bulletin at bpos %d, griblen is %d\0",
					bpos,*griblen);
			       dc_wclg(loglev,errgrp,numerr,errstr,&ier);
			       }
                        }
			else
			    finite_state = GRIB_IN_BULLETIN;
			break;

				
		}

	    }
        if((cret == EOD)&&(finite_state == GRIB_START))
           finite_state = GRIB_NOT_IN_BULLETIN;

	}

} 


