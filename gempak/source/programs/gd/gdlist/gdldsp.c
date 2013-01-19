#include "gdlist.h"

void gdldsp ( const char *gdfile, const char *gdtime1, const char *gdtime2,
              const int *level1, const int *level2, const int *ivcord,
	      const char *parm, const char *garea, const int *iscale,
	      const float *rmin, const float *rmax, const int *termflg,
	      const int *fileflg, int *iret )
/************************************************************************
 * gdldsp								*
 *									*
 * This subroutine allows the user to accept parameters for the GDLIST 	*
 * program.								*
 *									*
 * gdldsp ( gdfile, gdtime1, gdtime2, level1, level2, ivcord, parm,	*
 *          garea, iscale, rmin, rmax, termflg, fileflg, iret )		*
 *									*
 * Input parameters:							*
 *	*gdfile		const char	Grid file			*
 *	*gdtime1	const char	Grid time			*
 *	*gdtime2	const char	Grid time			*
 *	*level1		const int	Grid level			*
 *	*level2		const int	Grid level			*
 *	*ivcord		const int	Grid vertical coordinate	*
 *	*parm		const char	Grid parameter			*
 *	*garea		const char	Graphics area			*
 *	*iscale		const int	Scale factor			*
 *	*rmin		const float	Minimum data value		*
 *	*rmax		const float	Maximum data value		*
 *	*termflg	const int	Terminal output flag		*
 *	*fileflg	const int	File output flag		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  3 = user entered "EXIT"	*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	4/85						*
 * I. Graffman/RDS	8/86	Fixed return code after TM_ACCP		*
 * S. Schotz/GSC	6/90	Get respnd locally from IP_RESP		*
 * R. Tian/SAIC		9/06	Recoded from Fortran			*
 ************************************************************************/
{
    char output[21];
    int respnd, tltflg;
    int minus1, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    minus1 = -1;
    tltflg = G_TRUE;

    /*
     * Write out program name.
     */
    printf ( "\n\n GDLIST PARAMETERS: \n" );

    /*
     * Write out the grid file name.
     */
    printf ( "\n Grid file: %s\n\n", gdfile );

    /*
     * Write out the grid identifier.
     */
    printf ( " GRID IDENTIFIER:\n" );
    grc_wtrm ( stdout, &tltflg, &minus1, gdtime1, gdtime2, level1, level2,
        ivcord, parm, &ier );

    /*
     * Write out the area and output devices.
     */
    output[0] = '\0';
    if ( *termflg == G_TRUE ) strcpy ( output,                  " TERM/\n" );
    if ( *fileflg == G_TRUE ) strcpy ( &output[strlen(output)], " FILE/\n" );

    /*
     * Write out area, scale and output.
     */
    printf ( "\n GAREA:    %-48.48s\n SCALE FACTOR : 10**%2d\n OUTPUT:   %s\n",
	garea, *iscale, output );

    /*
     * Write out mininum and maximum values.
     */
    printf ( "\n MINIMUM AND MAXIMUM VALUES%9.2f%9.2f\n", *rmin, *rmax );

    /*
     * If respond is set, wait for user to accept parameters.
     */
    ip_resp ( &respnd, &ier );
    if ( respnd == G_TRUE ) {
	tm_accp ( &ier );
	if ( ier == 2 ) *iret = 3;
    }

    return;
}
