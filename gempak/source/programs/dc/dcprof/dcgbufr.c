#include "dccmn.h"

#ifdef UNDERSCORE
#define dc_gbufr	dc_gbufr_
#endif


/*
** Define and initialize the finite-state machine for finding a
** bulletin in the buffer.
*/

/* States for finding a bulletin */
typedef enum {
    BUFR_START,
    BUFR_NOT_IN_BULLETIN,
    BUFR_B_,
    BUFR_B_U_,
    BUFR_B_U_F_,
    BUFR_IN_BULLETIN,
    BUFR_7_,
    BUFR_7_7_,
    BUFR_7_7_7_,
    BUFR_COMPLETE
} fstategrib_t;


static fstategrib_t finite_state = BUFR_START;


void dc_gbufr ( gribbul, MAXGRIBSIZE, griblen, iret )
char	*gribbul;
int	*MAXGRIBSIZE;
int	*griblen;
int	*iret;

/************************************************************************
 * dc_ggrib								*
 *									*
 * This routine will check the buffer for GRIB data and return a 	*
 * pointer to the start of the GRIB bulletin as well as the length of	*
 * product. 		 						*
 *									*
 * dc_ggrib ( gribbul, griblen, iret )					*
 *									*
 * Output parameters:							*
 *	*gribbul	char		Pointer to start of GRIB	*
 *	 MAXGRIBSIZE    int		maximum size of gribbul		*
 *	*griblen	int		length of GRIB message		*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					 -60 = griblen too large	*
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
	static char errgrp[]="DCPROF";
	int loglev,numerr;

	int bpos=0;

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
**	Loop forever. The loop will be broken if a complete
**	bulletin is found.
*/
	while ( 1 ) {

/*
**	    Check for an empty buffer.
*/
	    if  ( ( finite_state != BUFR_START ) ||
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
		    return;
		}

	    }

/*
**	    Scan the input buffer.
*/
	    while ( ( cret = dcb_peekc ( &ch ) ) != EOD ) {
		switch ( finite_state ) {

		    case BUFR_START:
			if  ( ch == 'B' ) {
			    finite_state = BUFR_B_;
			}
			else {
			    finite_state = BUFR_NOT_IN_BULLETIN;
			}
			break;

/*
**		    If not inside a bulletin, look for BUFR
*/
		    case BUFR_NOT_IN_BULLETIN:
			if  ( ch == 'B' ) {
			    finite_state = BUFR_B_;
			}
			else {
			    finite_state = BUFR_NOT_IN_BULLETIN;
			    dcb_sbhd (0); /* don't let buffer overflow
					     while looking for start */
			}
			break;

		    case BUFR_B_:
			if  ( ch == 'U' ) {
			    finite_state = BUFR_B_U_;
			}
			else
			    {
			        finite_state = BUFR_NOT_IN_BULLETIN;
			}
			break;

		    case BUFR_B_U_:
			if  ( ch == 'F' ) {
			    finite_state = BUFR_B_U_F_;
			}
			else
			    {
			        finite_state = BUFR_NOT_IN_BULLETIN;
			    }
			break;

		    case BUFR_B_U_F_:
			if  ( ch == 'R' ) {
			    finite_state = BUFR_IN_BULLETIN;
			    dcb_sbhd ( -3 );
                            gribbul[0] = 'B';
                            gribbul[1] = 'U';
                            gribbul[2] = 'F';
                            gribbul[3] = 'R';
			    bpos=4;
			}
			else
			    {
			        finite_state = BUFR_NOT_IN_BULLETIN;
			    }
			break;

		    case BUFR_IN_BULLETIN:
		        gribbul[bpos] = ch; bpos++;
			if(bpos == 8)
                           {
			   dcb_sbtl ( 8 );
                           dcb_getb(bhead);
			   *griblen = gb_btoi(bhead,4,3,0);
			   if(*griblen > *MAXGRIBSIZE)
                              {
			      finite_state = BUFR_NOT_IN_BULLETIN;
                              loglev = 0; numerr = -50;
                              sprintf(errstr, "griblen [%d] > maxgribsize [%d]\0",
                                      *griblen,*MAXGRIBSIZE);
                              dc_wclg(loglev,errgrp,numerr,errstr,&ier);
			      return;
                              }
                           }
                        /* don't let buffer overflow */
                        if(bpos >8) dcb_sbhd (0); 
			if  ( ch == '7' ) {
			    finite_state = BUFR_7_;
			}
			break;

		    case BUFR_7_:
		        gribbul[bpos] = ch; bpos++;
			if  ( ch == '7' ) 
			    finite_state = BUFR_7_7_;
			else
			    finite_state = BUFR_IN_BULLETIN;
			break;

		    case BUFR_7_7_:
		        gribbul[bpos] = ch; bpos++;
			if  ( ch == '7' ) 
			    finite_state = BUFR_7_7_7_;
			else
			    finite_state = BUFR_IN_BULLETIN;
			break;

		    case BUFR_7_7_7_:
		        gribbul[bpos] = ch; bpos++;
			if( ch == '7' ) {
                            if(bpos >= *griblen)
                               {
                               if(bpos > *griblen)
                                  printf("bulletin too long %d > %d\n",
				          bpos,*griblen);
			       nbull++;
                               finite_state = BUFR_START;
			       return;
                               }
                        }
			else
			    finite_state = BUFR_IN_BULLETIN;
			break;

				
		}

	    }
        if((cret == EOD)&&(finite_state == BUFR_START))
           finite_state = BUFR_NOT_IN_BULLETIN;

	}

} 


