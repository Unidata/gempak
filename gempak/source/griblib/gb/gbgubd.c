#include "gbcmn.h"

void gb_gubd ( int *kx, int *ky, int *iuscal, int *misflg, 
		float *rmsval, int *nrmflg, int *irltv, int *nbits, 
		float *fgrid, int *iret )
/************************************************************************
 * gb_gubd                                                              *
 *                                                                      *
 * This function gets the GRIB data from GB_BDS and returns an unpacked *
 * grid of floating point numbers.                                      *
 *                                                                      *
 * gb_gubd ( kx, ky, iuscal, misflg, rmsval, nrmflg, irltv,		*
 *           nbits, fgrid, iret )					*
 *                                                                      *
 * Input parameters:							*
 *	*kx		int		Number of columns		*
 *	*ky		int		Number of rows			*
 *      *iuscal		int		GEMPAK units scale factor       *
 *	*misflg		int		Missing data flag		*
 *	*rmsval		float		Missing data value		*
 *	*nrmflg		int		"Normal" grid flag		*
 *					   i.e., the grid is NOT	*
 *					   rotated or stretched and	*
 *					   the scanning mode is OK	*
 *	*irltv		int		Resolution flag			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nbits		int		Number of bits for packing	*
 *      *fgrid		float		Unpacked grid data              *
 *      *iret		int		Return code                     *
 *					     +5 = BDS too short		*
 *                                          -16 = Error on next msg     *
 *					    -18 = Invalid bitmap	*
 *					    -22 = Invalid bds length	*
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI           7/93                                           *
 * S. Jacobs/EAI	10/93	Changed memmove to memcpy		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * K. Brill/NMC          6/94   Fixes for BITMAP grids          	*
 * L. Sager/NMC          8/95   Fix memory allocation error     	*
 * K. Brill/NMC		10/95	Do not increase bds.num_bits =1 	*
 * K. Brill/EMC		12/95	If nbits = 1 increment by 1		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 2/96	Cleanup GBDIAGs and comments		*
 * D.W.Plummer/NCEP	 3/96	Changes cfl_ call sequence		*
 * D.W.Plummer/NCEP	 8/96	Temporary ECMWF processing fix  	*
 * D.W.Plummer/NCEP	12/96	Added test for bdslength<=0 (iret=-22)	*
 * S. Jacobs/NCEP	 8/98	Changed igrid from long int to int	*
 * T. Piper/GSC		11/98	Updated prolog				*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 ***********************************************************************/
{
register int	i;
int		offset, ret, bitmaplength, leng, rmder,
		jshft, length, *misarr, m, n, idxarr, npts,
		ibeg, iinc, jbeg, jinc, bmshdr,
		idrct, jdrct, consec, icnt, jcnt, kcnt, nbytes, indx;
int		warning, *igrid;
unsigned char	*buffer, *bmptr, *indxb, kbit, *tmpbuf;
char		*tmpend;
float		ref;
float		*tmpgrd, *tmpgrd2;

/*---------------------------------------------------------------------*/

        *iret = 0;

	warning = *irltv; /* NOTE:  this input parameter is NOT used */
	warning++;
	/*
	 *	Read the BMS to get the bitmap.
	 */
	if ( *misflg == TRUE ) {

	    offset = cursor + ISLENGTH + pdslength + gdslength;
	/* 
	 *      Allocate space for the buffer.
	 */
        buffer = (unsigned char *)
                  malloc ( 3 * sizeof(unsigned char) );
	    cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	    cfl_read ( gbfile.fptr, 3, buffer, &nbytes, &ret );
	    if ( ret != 0 ) {
		*iret = -16;
		return;
	    }

	    indx = 0;
	    bmslength = gb_btoi ( buffer, indx, 3, FALSE );
            free ( buffer );
	/* 
	 *      Allocate space for the buffer.
	 */
        buffer = (unsigned char *)
                  malloc ( bmslength * sizeof(unsigned char) );
	    cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	    cfl_read ( gbfile.fptr, bmslength, buffer, &nbytes, &ret );
	    if ( ret != 0 ) {
		*iret = -16;
		return;
	    }

	    gb_bms ( buffer );

	    /*
	     *  Bitmap section header length is 6 bytes
	     */
	    bmshdr = 6;
	    bitmaplength = bmslength - bmshdr;

	    if ( bms.table == 0 ) {
		bmptr = ( unsigned char * )
		    malloc ( bitmaplength * sizeof ( unsigned char * ) );	
		indxb = bmptr;
		memcpy ( bmptr, &buffer[bmshdr], bitmaplength );
	    }
	    else {
	/*
	 *	    	Load the predetermined bit map provided by the center.
	 */
		*iret = -18;
		return;
	    }
            free ( buffer );

	}
	else {
	    bmslength = 0;
 	}

	/*
	 *	Read in the BDS to get the number of bits, the reference value,
	 *	and the data.
	 */
 	offset = cursor + ISLENGTH + pdslength + gdslength + bmslength;
	/* 
	 *      Allocate space for the buffer.
	 */
        buffer = (unsigned char *)
                  malloc ( 3 * sizeof(unsigned char) );
        
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
 	cfl_read ( gbfile.fptr, 3, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -16;
	    return;
	}

	indx = 0;
 	bdslength = gb_btoi ( buffer, indx, 3, FALSE );
        free ( buffer );
	if ( bdslength <= 0 )  {
	    *iret = -22;
	    return;
        }
	/* 
	 *  Allocate space for the buffer.
	 */
        buffer = (unsigned char *)
                  malloc ( bdslength * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, bdslength, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -16;
	    return;
	}

	length = (*kx) * (*ky);

	/*
	 *	Compute leng, also check for invalid bdslength.
	 */
	rmder = ( bdslength - 11 ) % sizeof (int);
	if ( rmder == 0 )
	    leng = ( bdslength - 11 ) / sizeof (int);
	else
	    leng = ( bdslength - 11 ) / sizeof (int) + 1;
	/*
	 *	Check for constant grid. Data section has length 12: 11 for
	 *	the header and 1 data byte.
	 */
	if ( leng == 1 )  {
	    igrid = ( int * ) malloc ( length * sizeof(int) );
        }
	else 
	    igrid = ( int * ) malloc ( leng * sizeof(int) );

	gb_bds ( buffer, length, igrid );

	/*
	 *  Read in End Section
	 */ 
	cfl_read ( gbfile.fptr, 4, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -16;
	    return;
	}

	gb_ends(buffer);

        free ( buffer );

	ref = bds.ref_value;

	*nbits = bds.num_bits;
	tmpgrd2 = (float *) malloc ( length * sizeof ( float ) );

	npts = length;

	if ( *misflg == TRUE ) {
	    misarr = ( int * ) malloc ( length * sizeof ( int ) );
	/*
	 *      Initialize missing value array to all 1's.
	 */
       	    for ( i = 0; i < length; i++ ) {
	          misarr[i] = 1;
	    }

	    if ( ERMISS(*rmsval) ) {
		*nbits = bds.num_bits + 1;
	    }

	/*
	 *	    Convert the bitmap to an integer array.
	 */
	    npts = 0; 
	    for ( i = 0; i < length; i++ ) {
		jshft = i % 8;
		if ( jshft == 0 ) {
		    kbit = *indxb;
		    indxb++;
		}			
		misarr[i] = ( ( kbit >> ( 8 - jshft - 1 ) ) & 1 ) ;
		if ( misarr[i] == 1 ) npts++;

	    }

	}

	/*
	 *	Unpack the buffered data into a floating point array.
	 */
	gb_unpk ( igrid, npts, bds.num_bits, tmpgrd2, iret );

	/*
	 *	Compute the values for the array, apply the scale and
	 *	reference value.
	 */
	m = 0;
	n = 0;
	while ( m < length ) {
	    if ( *misflg == TRUE && misarr[m] == 0 )
		fgrid[m] = *rmsval;
	    else {
		fgrid[m] = (float) ( (ref +
		  (tmpgrd2[n]*pow (2.0,(double)bds.binary_scale))) *
		    pow (10.0,(double)(*iuscal-pds.dec_scale)) );
		n++;
	    }

	    m++;

	}

	/*
	 *	If the grid is not "normal", change the indices of the array.
	 */
	if ( *nrmflg == FALSE ) {

	    tmpgrd  = (float *) malloc ( length * sizeof ( float ) );
	    for ( i = 0; i < length; i++ ) {
		tmpgrd[i] = fgrid[i];
		fgrid[i]  = 0;
	    }

	    idrct  = ( gds.scan_mode >> 7 ) & 1;
	    jdrct  = ( gds.scan_mode >> 6 ) & 1;
	    consec = ( gds.scan_mode >> 5 ) & 1;

	    /*
	     *  D.W.Plummer/NCEP 8/9/96 --
	     *  If European Center, force processing to adhere to
	     *  j-scan mode of zero.  This is due to an error in
	     *  the NCEP processing which will hopefully be 
	     *  corrected soon.
	     */
	    if ( pds.center == 98 )  {
		jdrct = 0;
	    }

	    if ( idrct == 0 ) {
		ibeg = 0;
		iinc = 1;
	    }
	    else {
		ibeg = *kx - 1;
		iinc = -1;
	    }
	    if ( jdrct == 1 ) {
		jbeg = 0;
		jinc = 1;
	    }
	    else {
		jbeg = *ky - 1;
		jinc = -1;
	    }

	    kcnt = 0;
	    if ( consec == 1 ) {
		for ( jcnt=jbeg; (0<=jcnt&&jcnt<*ky); jcnt+=jinc ) {
		    for ( icnt=ibeg; (0<=icnt&&icnt<*kx); icnt+=iinc ) {
			idxarr = *ky * icnt + jcnt;
			fgrid[kcnt] = tmpgrd[idxarr];
			kcnt++;
		    }
		}
	    }
	    else {
		for ( jcnt=jbeg; (0<=jcnt&&jcnt<*ky); jcnt+=jinc ) {
		    for ( icnt=ibeg; (0<=icnt&&icnt<*kx); icnt+=iinc ) {
			idxarr = *kx * jcnt + icnt;
			fgrid[kcnt] = tmpgrd[idxarr];
			kcnt++;
		    }
		}
	    }

	}
	/*
	 *	Bump nbits to 2 if it is 1.
	 */
	if ( *nbits == 1 ) *nbits = *nbits + 1;
	/*
	 *	Free up any dynamically allocated space.
	 */
	if ( *misflg == TRUE ) {
	    free ( misarr );
	    if ( bms.table == 0 ) {
		free ( bmptr );	
                }
	}

	if ( *nrmflg == FALSE ){
	    free ( tmpgrd );
        } 
        
	free ( tmpgrd2 );
	free (   igrid );

	if ( GBDIAG_BDS == TRUE )
	    printf("UNPACKED BINARY DATA\n" );

        offset = cursor + ISLENGTH +
                pdslength + gdslength + bmslength + bdslength;

        tmpbuf = (unsigned char *)
                  malloc ( 5 * sizeof(unsigned char) );
        cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
        cfl_read ( gbfile.fptr, 4, tmpbuf, &nbytes, &ret );
	tmpbuf[4] = CHNULL;
	tmpend = (char *) tmpbuf;
	strcpy ( es.end_string, tmpend );
	free ( tmpbuf );

        if ( ret != 0 ) {
            *iret = -19;
            return;
        }

	if ( GBDIAG_END == TRUE )
	    printf("END DATA SECTION = %s\n", es.end_string);

}
