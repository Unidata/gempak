#include "nccmn.h"

void mopen ( int *iret )
/************************************************************************
 * mopen								*
 *									*
 * This subroutine opens the output metafile for the device driver.	*
 *									*
 * mopen  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * C. Lin/EAI		10/92						*
 * C. Lin/EAI		11/92		Add Header			*
 * C. Lin/EAI		 4/93		Add Internal Buffer		*
 * A. Chang/EAI		 1/94		Added func arg filnam		*
 * A. Chang/EAI		 1/94		Modified to buffered I/O 	*
 * A. Chang/EAI		 4/94		Removed func arg filnam		*
 * A. Chang/EAI		 5/94		Eliminated printf statements	*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	 9/94		Do not append to file "Nmeta"	*
 * C. Lin/EAI		10/94	 	Reset text alignment 		*
 * S. Jacobs/NMC	 6/95		Changed file mode from 666->644	*
 ***********************************************************************/
{
	short		buffer[3];
	struct stat	statbuf;
	char		filtyp[3];
	int		i, jnum, ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Check for the existence of the metafile.
 */
	if ( ( stat ( curfil, &statbuf ) == -1 ) ||
	     ( strcmp ( curfil, "Nmeta" ) == 0 ) ) {

/*
 *	    Create a new file.
 */
	    exist = G_FALSE;
	    strcpy ( filtyp, "wb" );
	    buffer[0] = BEG_MF;
	    buffer[1] = BEG_PIC;
	    buffer[2] = BEG_PICBODY;
	    jnum = 3;
	}
	else {
/*
 *	    Use the existing file.
 */
	    exist = G_TRUE;
	    strcpy(filtyp, "r+");
	    buffer[0] = BEG_PIC;
	    buffer[1] = BEG_PICBODY;
	    jnum = 2;
	}


/*
 *	Open the file using the options set above.
 */
	if ( ( meta_fp = fopen ( curfil, filtyp ) ) == NULL ) {
	    *iret = G_NOMETA;
	    return;
	}

/* 
 *	Set the file permissions and setup the I/O buffer.
 */
	meta_id = fileno ( meta_fp );
	fchmod ( meta_id, 00644 );
	setvbuf ( meta_fp, ncbuffer, _IOFBF, MAX_BUFSZ );

/*
 *	Initialize the file header and counters.
 */

	if ( ! exist ) { 
/*
 *	    The file does not exist,...
 */
	    byte_count  = 0;
	    frame_count = 0;
	    maxfrm = MAX_NFRM;

	    for ( i = 0; i < MAX_NFRM; i++ ) {
		meta_frame[i].isbyte = 0;
		meta_frame[i].iebyte = 0;
		memset ( meta_frame[i].label, '\0', FRM_LBLSZ );
	    }

/*
 *	    write the file header, then...
 */
	    mwflhd ( &ier );
	    if ( ier != G_NORMAL ) {
		*iret = G_NOMETA;
		return;
	    }

/*
 *	    write the frame headers.
 */
	    mwfrhd ( &ier );
	    if ( ier != G_NORMAL ) {
		*iret = G_NOMETA;
		return;
	    }
	}
	else {
/*
 *	    The file does exist,...
 */
	    byte_count  = 0;
	    frame_count = 0;

/*
 *	    read the file header, then...
 */
	    mrflhd ( &ier );
	    if ( ier != G_NORMAL ) {
		*iret = G_NOMETA;
		return;
	    }

/*
 *	    read the frame headers.
 */
	    if ( meta_head.maxframe < MAX_NFRM ) {
		maxfrm = meta_head.maxframe;
	    }
	    else {
		maxfrm = MAX_NFRM;
	    }

	    mrfrhd ( &ier );
	    if ( ier != G_NORMAL ) {
		*iret = G_NOMETA;
		return;
	    }

/*
 *	    Position the file pointer to end of file.
 *	    (The pointer is set to two bytes before the end in order
 *	     to eliminate the existing END_MF code.)
 */
	    if ( fseek ( meta_fp, -2, SEEK_END ) != 0 ) {
		*iret = G_NOMETA;
		return;
	    }
	    if ( ( byte_count = ftell ( meta_fp ) ) == -1L ) {
		*iret = G_NOMETA;
		return;
	    }

	    if ( frame_count >= maxfrm ) {
/*
 *		The file is full, cannot append to it.
 */
		mclose ( &ier );
		*iret = G_NMAXFR;
		return;
	    }

	}

/*
 *	Write the beginning of frame codes to the file and update the 
 *	counters and open file flag.
 */
	fwrite ( buffer, NUM_BTSZ, jnum, meta_fp );
	byte_count += NUM_BTSZ * jnum;

	meta_frame[frame_count].isbyte = byte_count;
	opnfil = G_TRUE;
		
/*
 *	Set all of the attributes to G_RESET.
 */
	lcolor_set = G_RESET;
	lwidth_set = G_RESET;
	fcolor_set = G_RESET;
	fstyle_set = G_RESET;
	txfont_set = G_RESET;
	txsize_set = G_RESET;
	txalgn_set = G_RESET;

}
