#include "geminc.h"
#include "gemprm.h"

typedef struct
{
    char	type[9];
    char	name[9];
    int		prod;
    int		lvls;
    char	units[9];
    float	res;
    char	desc[21];
} Radtbl_t;

#define	NIDPRD_TBL	"nidprd.tbl"

void tb_nids ( const int *isrc, const int *iprod, char *prdnam,
		int *nlev, char *units, float *res, char *desc,
		int *iret )
/************************************************************************
 * tb_nids								*
 *									*
 * This subroutine returns information about the radar image from the	*
 *  NIDS product table file.						*
 *									*
 * void tb_nids ( isrc, iprod, prdnam, nlev, units, res, desc, iret )	*
 *									*
 * Input parameters:							*
 *	*isrc		const int	NIDS source ID			*
 *	*iprod		const int	NIDS product ID                 *
 *									*
 * Output parameters:							*
 *	*prdnam		char	Product name				*
 *	*nlev		int	Number of data levels			*
 *	*units		char	Data units				*
 *	*res		float	Data resolution in km			*
 *	*desc		char	Data description			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 +3 = No entry found for image	*
 *					 -7 = Could not open imtyp table*
 *					-14 = bad source ID		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/97						*
 * T. Piper/SAIC	04/07	Re-wrote in 'C'				*
 * T. Piper/SAIC	04/08	Added ALASKA and HAWAII mosaics		*
 ***********************************************************************/
{
    int			found, ier, ier1, loglev=2;
    char		ctyp[9], *grp="TB", line[128];
    size_t		ii;
    static int		nr;
    static FILE		*fptr=NULL;
    static Radtbl_t	*radtbl=NULL;
/*---------------------------------------------------------------------*/
/*
 *  Initialize output variables.
 */
    prdnam[0] = '\0';
    *nlev = 0;
    units[0] = '\0';
    *res = 0.0;
    desc[0] = '\0';
    *iret = 3;  /*  Not found  */

/*
 *  If table not yet read into memory; open the image type table file.
 */
    if ( fptr == NULL ) {
	fptr = cfl_tbop ( NIDPRD_TBL, "rad", &ier );
	if ( ier != 0 ) { 
	    *iret = -7;
	    er_lmsg (&loglev, grp, iret, NIDPRD_TBL, &ier1, strlen(grp), strlen(NIDPRD_TBL));
	    return;
	}

/*
 *  Load the structure.
 */
	cfl_tbnr (fptr, &nr, &ier );
	if ( ier != 0 || nr == 0 ) {
	    *iret = -7;
	    er_lmsg (&loglev, grp, iret, NIDPRD_TBL, &ier1, strlen(grp), strlen(NIDPRD_TBL));
	    cfl_clos(fptr, &ier);
	    return;
	}
	G_MALLOC(radtbl, Radtbl_t, nr, "tb_nids:  radtbl");
	for (ii = 0; ii < (size_t)nr; ii++ ) {

/*
 *  Read the next record.
 */
	    cfl_trln ( fptr, 127, line, &ier );
	    if ( ier == 0 ) {
		sscanf(line,"%s %s %i %i %s %f %s",
			radtbl[ii].type, radtbl[ii].name, &radtbl[ii].prod,
			&radtbl[ii].lvls, radtbl[ii].units,
			&radtbl[ii].res, radtbl[ii].desc);
	    }
	}  /*  end of 'for (ii = 0; ii < nr; ii++ )'  */
	cfl_clos(fptr, &ier);
    }  /*  end of 'if ( *fptr == NULL )'  */

/*
 *  Convert Site ID number to product type.
 */
    ii = 0;
    ctyp[0] = '\0';
    found = G_FALSE;
    if ( *isrc < 10000 ) {
	strcpy(ctyp, "LOCAL");
    }
    else if ( *isrc == 10000 ) {
	strcpy(ctyp, "NATIONAL");
    }
    else if ( *isrc >= 10001 && *isrc <= 10007 ) {
	strcpy(ctyp, "REGIONAL");
    }
    else if ( *isrc == 10051 ) {
	strcpy(ctyp, "ALASKA");
    }
    else if ( *isrc == 10052 ) {
	strcpy(ctyp, "HAWAII");
    }

    if ( ctyp[0] != '\0' ) {
	while ( !found && ii < (size_t)nr ) {
	    if ( strcmp(ctyp, radtbl[ii].type) == 0 ) {
		if ( *iprod == radtbl[ii].prod ) {
		    found = G_TRUE; 
		    strcpy(prdnam, radtbl[ii].name);
		    *nlev = radtbl[ii].lvls;
		    strcpy(units, radtbl[ii].units);
		    *res = radtbl[ii].res;
		    strcpy(desc, radtbl[ii].desc);
		    *iret = G_NORMAL;
	        }
	    }
	    ii++;
        }
    }
    else {
	*iret = -14;
    }
}
