#include <geminc.h>
#include <gemprm.h>
#include <dccmn.h>
#ifdef UNDERSCORE
#define nldnflush	nldnflush_
#endif

#include "dcnldn.h"


int read_int1(x,value)
    unsigned char *x;
    int          *value;
{

       *value = (x[0] << 24) | (x[1] << 16) | (x[2] << 8) | x[3];
       return 1;
}

int read_short1(x,value)
    unsigned char *x;
    short         *value;
{

       *value = (x[0] << 8) | x[1];
       return 1;
}

int read_char1(x, value)
    unsigned char *x;
    char         *value;
{
    *value = x[0]; 
    return 1;
}



/*
** Define and initialize the finite-state machine for finding a
** bulletin in the buffer.
*/

/* States for finding a bulletin */
typedef enum {
    NLDN_START,
    NLDN_NOT_IN_BULLETIN,
    NLDN_N_,
    NLDN_N_L_,
    NLDN_N_L_D_,
    NLDN_IN_BULLETIN
} fstatenldn_t;


static fstatenldn_t finite_state = NLDN_START;


void dc_gnldn ( int *nhead, int *nflash, int *ifdtyp, int *iret ) 
/************************************************************************
 * dc_gnldn								*
 *									*
 * This routine will check the buffer for NLDN data and return the 	*
 * header size and number of flash records				*
 * bulletin to the decoder.						*
 *									*
 * dc_gnldn ( bulletin, lenb, ifdtyp, iret )				*
 *									*
 * Output parameters:							*
 *	*bulletin	char		Bulletin			*
 *	*lenb		int		Length of bulletin		*
 *	*ifdtyp		int		Feed type of data		*
 *					  0 = WMO			*
 *					  1 = AFOS			*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *									*
 **									*
 * Log:									*
 * A. Chang/EAi		 6/95						*
 * S. Jacobs/NMC	 7/95	Clean up				*
 * S. Jacobs/NCEP	 4/96	Added AFOS data feed type		*
 * S. Jacobs/NCEP	 6/96	Updated documentation			*
 * E. Wehner/EAI	 7/96	Added more checking for state machine	*
 * J. Whistler/AWC	 8/96	Changed call to dcb_peekc		*
 ***********************************************************************/

{

	static unsigned char	cret, ch;
	char bhead[13];
	int			ier;
	int ichk;
	char errgrp[10],errstr[80];
	int loglev,numerr;

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
	    if  ( ( finite_state != NLDN_START ) ||
		  ( dcb_isempt() == 1 ) ) {


/*
**		Read from standard input to the buffer.
*/
		if  ( ( ier = dcb_put ( STDIN_FILENO, itmout ) ) < 0 ) {

/*
**		    Timeout or end of data file has occurred.
*/
                    loglev = 1; numerr = ier;
                    strcpy(errgrp,"DCGNLDN\0");
                    sprintf(errstr,"time out or end of data\0");
                    dc_wclg(loglev,errgrp,numerr,errstr,iret);
		    *iret = ier;
		    return;
		}
                /*if(finite_state == NLDN_START) 
                   printf("start empty buffer %d\n",ier);
                else
                   printf("isempt %d\n",ier);*/

	    }

/*
**	    Scan the input buffer.
*/
	    while ( ( cret = dcb_peekc ( &ch ) ) != EOD ) {
		switch ( finite_state ) {

/*
**		    If at the start, check for a Control-A or a "Z".
*/
		    case NLDN_START:
			if  ( ch == 'N' ) {
			    finite_state = NLDN_N_;
			}
			else {
			    finite_state = NLDN_NOT_IN_BULLETIN;
			}
			break;

/*
**		    If not inside a bulletin, check for a Control-A
**		    or a "Z".
*/
		    case NLDN_NOT_IN_BULLETIN:
			if  ( ch == 'N' ) {
			    finite_state = NLDN_N_;
			}
			else {
			    finite_state = NLDN_NOT_IN_BULLETIN;
			}
			break;

/*
**		    If Control-A has been found, check for a
**		    Carriage Return.
*/
		    case NLDN_N_:
			if  ( ch == 'L' ) {
			    finite_state = NLDN_N_L_;
			}
			else
			    {
			        finite_state = NLDN_NOT_IN_BULLETIN;
			}
			break;

/*
**		    If Control-A and Carriage Return have been found,
**		    check for another Carriage Return.
*/
		    case NLDN_N_L_:
			if  ( ch == 'D' ) {
			    finite_state = NLDN_N_L_D_;
			}
			else
			    {
			        finite_state = NLDN_NOT_IN_BULLETIN;
			    }
			break;

/*
**		    If Control-A and two Carriage Returns have been
**		    found, check for a Line Feed.
*/
		    case NLDN_N_L_D_:
			if  ( ch == 'N' ) {
			    dcb_sbhd ( -3 );
			    dcb_sbtl ( 8 );
			    finite_state = NLDN_START;
			    *ifdtyp = 2;
                            dcb_getb(bhead);
                            ichk = read_int1(bhead+4,nhead); 
                            ichk = read_int1(bhead+8,nflash);
                            return;
			}
			else
			    {
			        finite_state = NLDN_NOT_IN_BULLETIN;
			    }
			break;

		    case NLDN_IN_BULLETIN:
                        finite_state = NLDN_NOT_IN_BULLETIN;
			break;

				
		}

	    }
        if((cret == EOD)&&(finite_state == NLDN_START))
           finite_state = NLDN_NOT_IN_BULLETIN;

	}

} 

int peeknldn(charval)
char *charval;
{
int ier;
unsigned char cret,chvalue;

cret = dcb_peekc(&chvalue);

if(cret == EOD) 
   {
   if  ( ( ier = dcb_put ( STDIN_FILENO, itmout ) ) < 0 ) {
/*
**                  Timeout or end of data file has occurred.
*/
      printf("time out or end of data (peeknldn)\n");
      return(ier);
      }
    cret = dcb_peekc(&chvalue);
    }
*charval = (char)chvalue;
return(0);
}

#define INGESTORFLASHLEN	28
void dcnldn_input(nldn_file ltgf, int *retval)
{
char    bulletin[20000],*bpos;
int     ifdtyp;
int     iret;
int	value,nhead,nflash;
int	bufsize,fcnt,DONE;
char	chvalue;
short	shvalue;
int i, iflno;

nldn_flash flashdat;
char errgrp[10],errstr[80];
int loglev, numerr,ier;


dc_gnldn ( &nhead, &nflash, &ifdtyp, &iret );

if(iret != 0)
   {
   *retval = iret;
   return;
   }

loglev = 1; numerr = iret;
strcpy(errgrp,"DCGNLDN\0");
sprintf(errstr,"Header len %d nflash %d\0",nhead,nflash);

dc_wclg(loglev,errgrp,numerr,errstr,&ier);

bufsize = nhead * INGESTORFLASHLEN - 4;
dcb_sbhd ( -3 );
dcb_sbtl ( 0 );
iret = dcb_getb(bulletin);
for(i=0;i<bufsize;i++)
   {
   if((iret = peeknldn(&chvalue)) != 0)
      {
      loglev = 0; numerr = iret;
      strcpy(errgrp,"DCGNLDN\0");
      sprintf(errstr,"trouble peeking value in header\0");
      dc_wclg(loglev,errgrp,numerr,errstr,&ier);
      }
   else
      bulletin[i+4] = chvalue;
   }

loglev = 2; numerr = iret;
strcpy(errgrp,"DCGNLDN\0");
sprintf(errstr,"iret dcb_getb %d\0",iret);
dc_wclg(loglev,errgrp,numerr,errstr,&ier);


fcnt = 0; DONE = 0;
while((fcnt < nflash)&&(DONE == 0))
   {
   iret = 0;
   dcb_sbhd ( 0 );
   for(i=0;i<INGESTORFLASHLEN;i++)
      {
      if((iret += peeknldn(&chvalue)) != 0)
         {
         loglev = 0; numerr = iret;
         strcpy(errgrp,"DCGNLDN\0");
         sprintf(errstr,"trouble peeking value in flash %d\0",fcnt+1);
         dc_wclg(loglev,errgrp,numerr,errstr,&ier);
         }
      else
         bulletin[i] = chvalue;
      }
   
   loglev = 2; numerr = iret;
   strcpy(errgrp,"DCGNLDN\0");
   sprintf(errstr,"iret dcb_getb %d fcnt %d/%d\0",iret,fcnt,nflash);
   dc_wclg(loglev,errgrp,numerr,errstr,&ier);
   if(iret == 0)
      {
      bpos = bulletin;
      iret += read_int1(bpos,&flashdat.sec); bpos += 4;
      iret += read_int1(bpos,&flashdat.nsec); bpos += 4;
      iret += read_int1(bpos,&value); bpos += 4;
      flashdat.lat = value / 1000.0;
      iret += read_int1(bpos,&value); bpos += 4;
      flashdat.lon = value / 1000.0;

      iret += read_short1(bpos,&shvalue); bpos += 2; /* fill */

      iret += read_short1(bpos,&shvalue); bpos += 2;
      flashdat.sgnl = shvalue / 10.0;

      iret += read_short1(bpos,&shvalue); bpos += 2; /* fill */

      iret += read_char1(bpos,&chvalue); bpos += 1;
      flashdat.mult = chvalue;
      iret += read_char1(bpos,&chvalue); bpos += 1; /* fill */

      iret += read_char1(bpos,&chvalue); bpos += 1; 
      flashdat.semimaj = chvalue;
      iret += read_char1(bpos,&chvalue); bpos += 1;
      flashdat.eccent = chvalue;
      iret += read_char1(bpos,&chvalue); bpos += 1;
      flashdat.angle = chvalue;
      iret += read_char1(bpos,&chvalue); bpos += 1;
      flashdat.chisqr = chvalue;
      sprintf(errstr,"   sec %d nsec %d sgnl %f mult %d\n",
         flashdat.sec,flashdat.nsec,flashdat.sgnl,flashdat.mult);
      loglev = 4;
      dc_wclg(loglev,errgrp,numerr,errstr,&ier);


      iflno = write_nldn(ltgf, flashdat, &iret);
      if(iret == 0) nbull++;
      }
   else
      {
      DONE = 1;
      loglev = 0; numerr = iret;
      strcpy(errgrp,"DCGNLDN\0");
      sprintf(errstr,"Short record [%d], read %d of %d\0",fcnt+1,
         iret,INGESTORFLASHLEN);
      dc_wclg(loglev,errgrp,numerr,errstr,&ier);
      }
   fcnt++;
   }

if(iflno > 0) nldnflush(&iflno,&iret);

*retval = 0;

}
