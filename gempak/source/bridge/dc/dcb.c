/************************************************************************
 * DCB.C                                                                *
 *                                                                      *
 * This module deals with the decoder buffer which is a circular queue.	*
 * The last slot of the buffer is always empty to differentiate the     *
 * empty and full buffer. (i.e., the buffer can hold at most 		*
 * (DCMXBF - 1) bytes.)							*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 * int dcb_init ( void )                                        	*
 *                                                                      *
 *	Initialize the decoder buffer structure dcb.			*
 *                                                                      *
 *									*
 * size_t dcb_put ( fd, tmout )						*
 *	int fd; 		input file descriptor			*
 *	const int tmout; 	time out in seconds			*
 *									*
 *      Read data from a file to the decoder buffer and returns the     *
 *      number of bytes actually read and placed in the buffer.		*
 *									*
 *									*
 * unsigned char dcb_peekc ( ch )					*
 *									*
 *      Return  the character pointed by dcb.nxtpeek in the decoder     *
 *      buffer.								*
 *									*
 *									*
 * void dcb_sbhd ( offset )						*
 *	const int offset;	offset from the character just peeked.	*
 *									*
 *      Set bulletin head pointer from the character just peeked.       *
 *  	Discard the data before dcb.bhead. offset > 0 -- forward,       *
 *	offset < 0 -- backward.	 					*
 *									*
 *									*
 * void dcb_sbtl ( offset )						*
 *	const int offset;	offset from the character just peeked.	*
 *									*
 *      Set bulletin tail pointer from the character just peeked.	*
 *		offset > 0 : forward					*
 *		offset < 0 : backward					*
 *									*
 *									*
 * int dcb_getb ( bulletin )						*
 *	char *bulletin;		output bulletin buffer			*
 *									*
 *      Copy a bulletin from the decoder buffer.			*
 *                                                                      *
 *                                                                      *
 * int dcb_isempt ( void )						*
 *	Test if decoder buffer is empty.				*
 *		1 = empty, 0 = not empty				*
 *                                                                      *
 *                                                                      *
 * int dcb_isfull (void )						*
 *	Test if decoder buffer is full.					*
 *		1 = full, 0 = not full					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		 7/95	Modify from A. Chang's double buffer	*
 * E. Wehner/EAI	 7/96	Added debug logging to dcb_put		*
 * S. Jacobs/NCEP	 8/96	Changed debug logging from level 0 to 2	*
 * J. Whistler/AWC	 8/96	Changed dcb_peekc to return error code	*
 *				separate from data			*
 * S. Jacobs/NCEP	10/96	Added type cast for old_nxtfeed		*
 * K. Tyle/GSC		 1/97	Changed numerr = 0 to 2, with errgrp = 	*
 *				"DC" in dc_wclg calls			*
 * S. Jacobs/NCEP	 4/98	Fixed call sequence for the select func	*
 * S. Jacobs/NCEP	 3/99	Changed switch to if-else in dcb_put	*
 ***********************************************************************/
#include "dccmn.h"

#define DEBUG 1

static dcb_t dcb;
static char  init_flag = 0;


int dcb_init ( void )
/************************************************************************
 * dcb_init								*
 *	Initialize the decoder buffer.					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	dcb.sbuf = (char *) malloc ( DCMXBF );

	if  ( dcb.sbuf == NULL )  return ( EBNMEM );

	dcb.ebuf    = dcb.sbuf + DCMXBF - 1;
	dcb.bhead   = dcb.sbuf;
	dcb.btail   = dcb.sbuf;
	dcb.nxtfeed = dcb.sbuf;
	dcb.nxtpeek = dcb.sbuf;

	init_flag = 1;

	return ( 0 );

}

/*=====================================================================*/

size_t dcb_put ( int fd, int tmout )
/************************************************************************
 * dcb_put								*
 *	Read data from a file to the decoder buffer and returns the 	*
 *	number of bytes actually read and placed in the buffer. 	*
 *									*
 * size_t dcb_put ( fd, tmout )						*
 *									*
 * Input parameters:							*
 *	fd	int	input file descriptor ( assumed valid)		*
 * Output parameters:							*
 *	tmout	int	timeout in seconds for select			*
 **									*
 * 									*
 ***********************************************************************/
{

	int     more;
	int     maxfd; 	/* max number of descriptor */ 
	fd_set  readfds;
	int 	nread, try_read, readcnt;
	int     ret, ier;

#ifdef DEBUG
 	char 	errmsg[80];
        long 	old_nxtfeed;
#endif

	struct timeval  t;

/*---------------------------------------------------------------------*/

	/*
	 * initialize the decoder structure if necessary
	 */
	if  ( !init_flag ) 
	    if  ( ( ret = dcb_init ( ) ) != 0 )  return ret; 

	/*
    	 * Initialize input(or read) descriptor set.
    	 */
	FD_ZERO ( &readfds );		/* clear all bits in readfds */
	FD_SET  ( fd, &readfds );	/* turn on bit in readfds */

	/*
	 * block and wait for the input
	 */
	maxfd = fd +1;

	if  ( tmout > 0 )
	    t.tv_sec = (long)tmout;
	else 
	    t.tv_sec = 0;

	t.tv_usec = 0;
	more = select ( maxfd, &readfds, NULL, NULL, &t );

	/*
	 * read data into the buffer
	 */
	readcnt = 0;

	if  ( more == -1 )  {
	    return ( EBREAD );		/* error */
	}
	else if  ( more == 0 )  {
	    return ( EBTMOUT );		/* timeout */
	}
	else {				/* data coming */ 	
		/*
		 * check if buffer is full
		 */
		if  ( (dcb.nxtfeed + 1) == dcb.bhead )
                {
		    sprintf(errmsg, "Buffer full.  bhead->%li  nxtfeed->%li \n", 
                                    (long)(dcb.bhead-dcb.sbuf), 
				    (long)(dcb.nxtfeed-dcb.sbuf));

		    return( EBFULL); 
                }

		if  ( dcb.nxtfeed >= dcb.bhead ) {

		    if  ( dcb.bhead > dcb.sbuf ) {
				
			/*
			 * try to fill from dcb.nxtfeed to
			 * the end of the buffer.
			 */
			try_read = dcb.ebuf - dcb.nxtfeed + 1;
			nread    = read(fd, dcb.nxtfeed, try_read);
#ifdef DEBUG
                        old_nxtfeed = (long)dcb.nxtfeed;
#endif

			if  ( nread < 0 ) {
#ifdef DEBUG
                            sprintf(errmsg, 
                            "failed to read to pos-> %li for % i bytes \n",
                                      (long)(dcb.nxtfeed-dcb.sbuf), try_read);
                    	    dc_wclg(2, "DC", 2, errmsg, &ier);
#endif
			    return ( EBREAD );
			}
			else {
			    dcb.nxtfeed += nread;
			    readcnt     += nread;
			}
#ifdef DEBUG
                        sprintf(errmsg, 
                            "read %i/%i bytes strt %li newstrt %li\n",
		  	    nread, try_read, old_nxtfeed-(long)dcb.sbuf, 
			    (long)(dcb.nxtfeed-dcb.sbuf));
                    	dc_wclg(2, "DC", 2, errmsg, &ier);
#endif

			/*
			 * adjust dcb.nxtfeed and read more
			 * try to fill from the beginning of the buffer
			 * to dcb.bhead.
			 */
			if  ( dcb.nxtfeed > dcb.ebuf ) {
			    dcb.nxtfeed = dcb.sbuf;

			    try_read = dcb.bhead - dcb.nxtfeed - 1;
#ifdef DEBUG
                            old_nxtfeed = (long)dcb.nxtfeed;
#endif
			    nread = read(fd, dcb.sbuf, try_read);

			    if  ( nread < 0 ) {
#ifdef DEBUG
                            sprintf(errmsg, 
                            "failed to read to pos-> %li for % i bytes \n",
                                      (long)(dcb.nxtfeed-dcb.sbuf), try_read);
                    	    dc_wclg(2, "DC", 2, errmsg, &ier);
#endif
				return ( EBREAD );
			    }
			    else {
				dcb.nxtfeed += nread;
				readcnt     += nread;
			    }
#ifdef DEBUG
                            sprintf(errmsg, 
                                "read %i/%i bytes strt %li newstrt %li\n",
		  		nread, try_read, old_nxtfeed-(long)dcb.sbuf, 
				(long)(dcb.nxtfeed-dcb.sbuf));
                    	    dc_wclg(2, "DC", 2, errmsg, &ier);
#endif

			}
		    }
		    else {	/* dcb.bhead == dcb.sbuf */

			/*
			 * try to fill from dcb.nxtfeed to
			 * the end of the buffer.
			 */
			try_read = dcb.ebuf - dcb.nxtfeed;
#ifdef DEBUG
                        old_nxtfeed = (long)dcb.nxtfeed;
#endif
			nread    = read(fd, dcb.nxtfeed, try_read);

			if ( nread < 0 )
                        {
#ifdef DEBUG
                            sprintf(errmsg, 
                            "failed to read to pos-> %li for % i bytes \n",
                                      (long)(dcb.nxtfeed-dcb.sbuf), try_read);
                    	    dc_wclg(2, "DC", 2, errmsg, &ier);
#endif
		            return ( EBREAD );
        		}
			else {
			    dcb.nxtfeed += nread;
			    readcnt     += nread;
			}
#ifdef DEBUG
                        sprintf(errmsg, "read %i/%i bytes strt %li newstrt %li\n",
		  		nread, try_read, old_nxtfeed-(long)dcb.sbuf, 
				(long)(dcb.nxtfeed-dcb.sbuf));
                    	dc_wclg(2, "DC", 2, errmsg, &ier);
#endif

		    }

		}

		else {		/* dcb.nxtfeed < dcb.bhead */

		    try_read = dcb.bhead - dcb.nxtfeed - 1;
#ifdef DEBUG
                    old_nxtfeed = (long)dcb.nxtfeed;
#endif
		    nread = read(fd, dcb.nxtfeed, try_read);
		    if  ( nread < 0 ) {
#ifdef DEBUG
                        sprintf(errmsg, 
                        "failed to read to pos-> %li for % i bytes \n",
                                  (long)(dcb.nxtfeed-dcb.sbuf), try_read);
                    	dc_wclg(2, "DC", 2, errmsg, &ier);
#endif
			return ( EBREAD );
		    }
		    else {
			dcb.nxtfeed += nread;
			readcnt     += nread;
		    }
#ifdef DEBUG
                    sprintf(errmsg, "read %i/%i bytes start %li newstart %li \n",
			nread, try_read, old_nxtfeed-(long)dcb.sbuf, 
			(long)(dcb.nxtfeed-dcb.sbuf));
                    dc_wclg(2, "DC", 2, errmsg, &ier);
#endif


		}

		if  ( readcnt == 0 )  {
		    return ( EBEND );
		}
		else {
		    return ( readcnt );
		}

	}

}

/*=====================================================================*/

unsigned char dcb_peekc ( unsigned char *ch )
/************************************************************************
 *  dcb_peekc								*
 *									*
 * unsigned char dcb_peekc ( ch )					*
 *									*
 * Input parameters:							*
 *	*ch		unsigned char	Character to peek at		*
 * Output parameters:							*
 *	dcb_peekc	unsigned char	Retuned character		*
 **									*
 ***********************************************************************/
{


/*---------------------------------------------------------------------*/

	if ( dcb.nxtpeek == dcb.nxtfeed ) { 
	    return ( EOD );			/* end of data */
	}
	else { 
	    *ch = *dcb.nxtpeek++;
	    if  ( dcb.nxtpeek > dcb.ebuf ) 
		dcb.nxtpeek = dcb.sbuf;

	    return ( 1 );
	}

}

/*=====================================================================*/

void dcb_sbhd ( int offset ) 
/************************************************************************
 *  dcb_sbhd								*
 *	Set bulletin head pointer from the dcb.nxtpeek. Discard the 	*
 *	data before dcb.bhead.						*
 *									*
 * void dcb_sbhd ( offset )						*
 *									*
 * Input parameters:							*
 *	offset		int						*
 **									*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	/*
	 * figure out the head position dcb.bhead
	 */
	if  ( offset == 0 ) {
	    dcb.bhead = dcb.nxtpeek - 1;
	}
	else {
	    if ( offset >  0) 
	    	dcb.bhead = dcb.sbuf + 
	    	    ( dcb.nxtpeek - 1 - dcb.sbuf + offset ) % DCMXBF; 
	    else {
	        dcb.bhead = dcb.sbuf + ( 
	            ( dcb.nxtpeek - 1 - dcb.sbuf + offset
				+ DCMXBF ) % DCMXBF );
	    }
	}

}

/*=====================================================================*/

void dcb_sbtl ( int offset )
/************************************************************************
 *  dcb_sbtl								*
 *	Set bulletin tail pointer from the dcb.nxtpeek.			*
 *									*
 * void dcb_sbtl ( offset )						*
 *									*
 * Input parameters:							*
 *	offset		int						*
 **									*
 ***********************************************************************/
{ 

/*---------------------------------------------------------------------*/
	if  ( offset == 0 )  {
	    dcb.btail = dcb.nxtpeek - 1;
	}
	else {
	
	    if  ( offset >  0) 
	        dcb.btail = dcb.sbuf + 
	    	    ( dcb.nxtpeek - 1 - dcb.sbuf + offset ) % DCMXBF; 
	    else
	        dcb.btail = dcb.sbuf + 
	            ( dcb.nxtpeek - 1 - dcb.sbuf + offset
				+ DCMXBF ) % DCMXBF;
	}

}

/*=====================================================================*/

int dcb_getb ( char *bulletin )
/************************************************************************
 *  dcb_getb								*
 *	Copy a bulletin from the decoder buffer.			*
 *									*
 * int dcb_getb ( bulletin )						*
 *									*
 * Input parameters:							*
 *	*bulletin		char		Bulletin to copy	*
 * Output parameters:							*
 *	dcb_getb		int					*
 **									*
 ***********************************************************************/
{

	int	size, tmsize;

/*---------------------------------------------------------------------*/

	if  ( dcb.bhead == dcb.btail )  return ( EBEMPT );

	if  ( dcb.btail > dcb.bhead ) {
	    size = dcb.btail - dcb.bhead + 1;
	    memcpy(bulletin, dcb.bhead, size );
	}
	else {
	    /*
	     * copy from the bulletin head to the
	     * end of the buffer.
	     */
	    size = dcb.ebuf - dcb.bhead + 1;
	    memcpy(bulletin, dcb.bhead, size );

	    /*
	     * copy from the beginning of the buffer
	     * to the tail of the bulletin
	     */
	    tmsize = dcb.btail - dcb.sbuf + 1;
	    memcpy(bulletin + size, dcb.sbuf, tmsize );
	    size += tmsize;
	}

	/*
	 * adjust bulletin head and tail pointer
	 */
	if  ( ++dcb.btail > dcb.ebuf )  dcb.btail = dcb.sbuf; 

	dcb.bhead = dcb.btail;

	return size;

}

/*=====================================================================*/

int dcb_isempt ( void )
/************************************************************************
 *  dcb_isempt								*
 *									*
 * int dcb_isempt ( )							*
 *									*
 * Output parameters:							*
 *	dcb_isempt		int					*
 **									*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	if ( dcb.nxtfeed == dcb.bhead ) 
	    return (1);
	else 
	    return (0);
}

/*=====================================================================*/

int dcb_isfull ( void )
/************************************************************************
 *  dcb_isfull								*
 *									*
 * int dcb_isfull ( )							*
 *									*
 * Output parameters:							*
 *	dcb_isfull		int		Full = 1		*
 **									*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	if ( ( (long)dcb.nxtfeed + 1 ) % DCMXBF == (long)dcb.bhead ) 
	    return (1);
	else 
	    return (0);

}
