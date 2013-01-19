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

void tb_nidsdb ( char *prdnam, char *res, int *iprod, int *iret )
/************************************************************************
 * tb_nidsdb								*
 *									*
 * This subroutine returns information about the radar image from the	*
 * NIDS product table file.						*
 *									*
 * void tb_nidsdb (  prdnam, res, iprod, iret )				*
 *									*
 * Input parameters:							*
 *	*prdnam		char		Product name			*
 *	*res		char		Data resolution in km		*
 *									*
 * Output parameters:							*
 *	*iprod		int		NIDS product ID                 *
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 +3 = No entry found for image	*
 *					 -7 = Could not open imtyp table*
 **									*
 * Log:									*
 * m.gamazaychikov/CWS	01/10	Created					*
 ***********************************************************************/
{
    int			found, ier, ier1, loglev=2;
    float               ares;
    char		*grp="TB", line[128];
    size_t		ii;
    static int		nr;
    static FILE		*fptr=NULL;
    static Radtbl_t	*radtbl=NULL;
/*---------------------------------------------------------------------*/
/*
 *  Initialize output variables.
 */
    *iprod = 0;
    *iret = 33;  /*  Not found  */
    cst_crnm ( res, &ares, &ier );

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
	G_MALLOC(radtbl, Radtbl_t, nr, "tb_nidsdb:  radtbl");
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

    ii = 0;
    found = G_FALSE;

/*
 *  Retrieve the product ID for the specified 
 *  product name and the resolution.
 */
    while ( !found && ii < (size_t)nr ) {
      //if ( strstr (radtbl[ii].name, prdnam) != NULL ) {
      if ( strcmp (prdnam, radtbl[ii].name) == 0 ) {
         if ( G_DIFF(ares, radtbl[ii].res) ) {
	    found = G_TRUE; 
	    *iprod = radtbl[ii].prod;
	    *iret = G_NORMAL;
         }
      }
      ii++;
    }
}
