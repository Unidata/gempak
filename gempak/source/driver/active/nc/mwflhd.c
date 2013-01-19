#include "nccmn.h"

void mwflhd ( int *iret )
/************************************************************************
 * mwflhd								*
 *									*
 * This subroutine writes the file header to the metafile.		*
 *									*
 * mwflhd ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 * R. Tian/SAIC		 4/02		Add frame size, changed version *
 ***********************************************************************/
{
	int	length;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the file title.
 *	Fill the unused part of the title with NULLs.
 */
	strcpy ( meta_head.title, NC_TITLE );
	length = strlen ( meta_head.title ) + 1;
	memset ( ( meta_head.title + length ), '\0', 
		 ( sizeof ( meta_head.title ) - length ) ); 

/*
 *	Set the maximum number of frames for the file.
 */
	meta_head.maxframe = maxfrm;

/*
 *	Set the frame size
 */
	meta_head.fxsize = fxsize;
	meta_head.fysize = fysize;

/*
 *	Set the metafile version number for the file.
 */
	if ( meta_head.fxsize > 0 || meta_head.fysize > 0 ) {
	    meta_head.version = 2;
	} else {
	    meta_head.version = 1;
	}

/*
 *	Set the machine type for the file.
 */
	meta_head.machtype = MTMACH;

/*
 *	Set the reserved part of the file header to NULLs.
 */
	memset ( meta_head.reserved, '\0', FIL_RESSZ );

/*
 *	Write the file header to the file.
 */
	fwrite ( &meta_head, FIL_HDRSZ, 1, meta_fp );

/*
 *	Set the byte count.
 */
	byte_count += FIL_HDRSZ;

}
