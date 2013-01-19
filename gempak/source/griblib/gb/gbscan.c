#include "gbcmn.h"

void gb_scan ( FILE *fnfptr, FILE *infptr, int *ivers, int *iret )
/************************************************************************
 * gb_scan								*
 *									*
 * This function will scan the file and set the absolute address of	*
 * the next GRIB message.						*
 *									*
 * gb_scan ( fnfptr, infptr, ivers, iret )				*
 *									*
 * Input parameters:							*
 *	*fnfptr		FILE		The GRIB file number		*
 *	*infptr		FILE		The INDEX file number		*
 *									*
 * Output parameters:							*
 *	*ivers		int		GRIB version number		*
 *	*iret		int		Return code			*
 *					   5 = GRIB Version 0		*
 *					 -16 = no more messages		*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * J. Chou/EAI		07/93						*
 * S. Jacobs/EAI	11/93		Added return of version number	*
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * D.W.Plummer/NCEP	 3/96		Change cfl_ call sequence	*
 * T. Piper/GSC		11/98		Updated prolog			*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * T. Piper/SAIC	10/02	Added support for GRIB2			*
 ***********************************************************************/
{
	int		found, indx, temp, nbytes;
	long		offset;
	unsigned char	buff[BUFFSIZE];
  
/*---------------------------------------------------------------------*/
	*iret = 0;
	
	if ( infptr == NULL ) {	
/*
**	    The index file is not being used, or it does not
**	    exist, therefore scan the main GRIB file.
**
**	    Set the file pointer to the current position.
*/ 
	    cfl_seek ( fnfptr, cursor1, SEEK_SET, iret );
	    cfl_srch ( fnfptr, "GRIB", G_FSRCH, iret );
/*
**	    Find the start of the GRIB message.
*/ 
	    found = FALSE;
	    if ( *iret == G_NORMAL ) {
		    cfl_wher ( fnfptr, &offset, iret );
		    cfl_read ( fnfptr, 8, buff, &nbytes, iret );
		    if ( (int)buff[7] == 0 || (int)buff[7] == 1 
			|| (int)buff[7] == 2 ) {
			cfl_wher ( fnfptr, &offset, iret );
			offset = offset - 8;
		        *ivers = (int) buff[7];
		    	found = TRUE;
		    }

		    if ( found ) {
/*
**		    	Found the beginning of the message (GRIBxxxV)
**		    	in the buffer, where V is the version number.
**
**		    	Set the starting address of the message.
	offset is the position of the current "GRIB" string in the file
	cursor  is the position at the beginning of the current grib message
	cursor1 is the position at the end of the current grib message
*/
		    	if ( *ivers == 0 ) {
	    		    *iret = 5;
		    	}
		    	else if ( *ivers == 1 ) {
			    indx = 4;
			    temp = gb_btoi ( buff, indx, 3, FALSE );
			    cursor1 = offset + temp;
			    cursor = offset;
			    *iret = 0;
			}
			else if ( *ivers == 2 ) {
			    cfl_read ( fnfptr, 8, buff, &nbytes, iret );
			    indx = 4;
			    temp = gb_btoi ( buff, indx, 4, FALSE );
			    cursor1 = offset + temp;
			    cursor = offset;
			    *iret = 0;
		        }

		    }
	    }
	    if ( *iret != G_NORMAL  ||  !found )  {
/*
**		    Did not find the beginning of a message in the
**		    buffer.
*/
		    *iret = -16; 
	     }
	}
	else {

	    /*
	     *	The index file is being used.
	     */ 

	    if ( infile.curmsg < infile.nmsgs ) {
	    	cursor  = infile.msg[infile.curmsg].position;
	    	cursor1 = infile.msg[infile.curmsg].position + 
			  infile.msg[infile.curmsg].length;
	    	*ivers  = infile.msg[infile.curmsg].version;
	    	infile.curmsg++;
		if ( *ivers == 0 ) {
	    		*iret = 5;
		}
	    }
	    else {
		*iret = -16; 
	    }
	    
	}

}
