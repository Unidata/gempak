#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "shncmn.h"

void shn_clin ( void )
/************************************************************************
 * shn_clin( )								*
 *									*
 * This code logic was copied directly from that of the CLIPVGF utility	*
 * ($GEMPAK/source/programs/util/clipvgf/clipvgf.c).  It is needed in	*
 * order to prepare for future calls to subroutine SHN_DFHR.		* 
 *									*
 **                                                                     *
 *  Log:                                                                *
 *  J. Ator/NCEP        04/05                                           *
 ***********************************************************************/
{
	char device[8], dfilnam[128], pro[32];
	float xsize, ysize, lllat, lllon, urlat, urlon;
	float prjang1, prjang2, prjang3;
	int mode, istat, iunit, itype;
	int ier;
/*---------------------------------------------------------------------*/
	mode = 1;
	ginitp ( &mode, &istat, &ier );

	strcpy ( device, "GN" );

	iunit = 1;
	strcpy ( dfilnam, "CLIPVGF" );
	itype = 1;
	xsize = 500.0F;
	ysize = 500.0F;

	gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
       		 strlen(device), strlen(dfilnam));

	lllat = 0.0F;
	lllon = -135.0F;
	urlat = 0.0F;
	urlon = 45.0F;
	strcpy ( pro, "str" );
	prjang1 = 90.0F;  prjang2 = -105.0F;  prjang3 = 0.0F;
	gsmprj ( pro, &prjang1, &prjang2, &prjang3,
        	 &lllat, &lllon, &urlat, &urlon, &ier, strlen(pro));
}
