#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

CLO_t 	clo;

int     nhot, hotlist[MAXHOT];
long    hotstrec[MAXHOT];
int	_mxspts, _mxbpts;

int	whichStn;
int	sortStn;
int	npStn;
float	*latStn, *lonStn;

int	whichBnd;
int	sortBnd;
int	npBnd;
float	*xpBnd, *ypBnd;
float	latllBnd, lonllBnd, laturBnd, lonurBnd;
int	boundBnd, bndsptBnd;
char	*tBndName, *tBndData;
FILE	*fpBnd;

#define	CLO_TBL	"clo.tbl"	/* CLO library tables table	*/

void clo_init ( int *iret )
/************************************************************************
 * clo_init								*
 *									*
 * This function initializes values for the clo library.		*
 *									*
 * clo_init ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/97	Created					*
 * D.W.Plummer/NCEP	11/97	Added local global polygon info		*
 * F. J. Yen/NCEP	09/98	Updated for new clo_ancrd & SFSTN type	*
 * D.W.Plummer/NCEP	12/98	Updated for CITY type			*
 * D.W.Plummer/NCEP	 1/99	Updated for re-done clocmn.h include 	*
 * D.W.Plummer/NCEP	 2/99	Added clo_rdcntyb			*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE and COASTAL types	*
 * D.W.Plummer/NCEP	 4/01	Added global variables for bnds search	*
 * T. Piper/SAIC        12/01   Close file                              *
 * D.W.Plummer/NCEP	 6/01	Added initialization of tag variables	*
 * B. Yin/SAIC	 	 7/04	Changed clo_rdstn calling sequences	*
 * J. Wu/SAIC	 	 6/05	refer to _mxspts/_mxbpts & use G_MALLOC	*
 ***********************************************************************/
{
int	ii, nchar, nlocs, bufsiz, format, ier;
char	buffer[80], name[32], file[64];
FILE	*fp;
static	int	readCLO = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( readCLO != 0 )  return;

    nhot = 0;

    fp = cfl_tbop ( CLO_TBL, "config", &ier );

    if ( ier == 0 )  {

        bufsiz = sizeof ( buffer );

	cfl_tbnr ( fp, &nlocs, &ier );

	clo.loc = (CLOinfo_t *)malloc( (size_t)nlocs * sizeof(CLOinfo_t) );

	_mxspts = 0;
	_mxbpts = 0;
	clo.nloc = 0;
        for ( ii = 0; ii < nlocs; ii++ )  {
    
            cfl_trln ( fp, bufsiz, buffer, &ier );
	    sscanf ( buffer, "%s %d %s", name, &format, file );

	    G_MALLOC ( clo.loc[clo.nloc].name, char, (int)strlen(name)+1, "CLO_INIT" );
	    strcpy ( clo.loc[clo.nloc].name, name );
	    clo.loc[clo.nloc].format = format;
	    G_MALLOC ( clo.loc[clo.nloc].file, char, (int)strlen(file)+1, "CLO_INIT" );
	    strcpy ( clo.loc[clo.nloc].file, file );

	    switch ( format )  {
		case	0:			/* station file format  */
		    clo_rdstn( &(clo.loc[clo.nloc].stn), 
				clo.loc[clo.nloc].file, name, &ier );
	    	    if ( ier == 0 )  {
		    	_mxspts = G_MAX ( _mxspts, clo.loc[clo.nloc].stn.nstn );
			clo.nloc++;
		    }
		    break;
		case	1:			/* bounds file format	*/
		    clo_rdbnd( &(clo.loc[clo.nloc].bnd), 
				clo.loc[clo.nloc].file, &ier );
	    	    if ( ier == 0 )  {
		        _mxbpts = G_MAX ( _mxbpts, clo.loc[clo.nloc].bnd.maxpts );
			clo.nloc++;
		    }
		    break;
		default:
		    ier = -1;
		    break;
	    }

        }

        cfl_clos(fp, &ier);

    }

    G_MALLOC ( latStn, float, _mxspts, "CLO_INIT" );
    G_MALLOC ( lonStn, float, _mxspts, "CLO_INIT" );
    npStn = 0;
    whichStn = -1;
    sortStn = -1;

    G_MALLOC ( xpBnd, float, _mxbpts, "CLO_INIT" );
    G_MALLOC ( ypBnd, float, _mxbpts, "CLO_INIT" );
    npBnd = 0;
    whichBnd = -1;
    sortBnd = -1;
    latllBnd = -90.0F; lonllBnd = -180.0F;
    laturBnd =  90.0F; lonurBnd =  180.0F;
    boundBnd = 0;
    bndsptBnd = 0;
    fpBnd = (FILE *)NULL;
    nchar = 80;
    G_MALLOC ( tBndName, char, nchar, "CLO_INIT" );
    G_MALLOC ( tBndData, char, nchar, "CLO_INIT" );
    
    readCLO = 1;
    
}
