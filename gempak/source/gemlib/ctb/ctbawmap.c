#include "geminc.h"
#include "gemprm.h"

void ctb_awmap ( int pick, int *kxsize, int *kysize, int ilat[], 
				int ilon[], int iangles[], int *iret )
/************************************************************************
 * ctb_awmap								*
 *									*
 * This function reads the AWIPS product definition tables for the 	*
 * requested product.							*
 *									*
 * ctb_awmap ( pick, kxsize, kysize, ilat, ilon, iangles, iret )	*
 *									*
 * Input parameters:							*
 *	pick            int 		AWIPS map ID                    *
 *									*
 * Output parameters:							*
 *	*kxsize         int             Reference M coordinate          *
 *	*kysize		int             Reference N coordinate          * 
 *	ilat [ ]        int             Latitude points for map proj.   *
 *	ilon [ ]        int             Longitude points for map proj.  *
 *	iangles [ ]     int             Central latitudes for map proj. *
 *	*iret		int		Return code			*
 *					  -1 = table cannot be opened	*
 *					  -2 = entry not found		*
 **									*
 * Log:									*
 * A. Hardy/GSC	 	 9/98	Copied from CTB_AFOS			*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{

	FILE   *ftbl;
	char   tblnam[72], dirsym[72], buffer[133];
	int    found, ikxsize, ikysize;
        int    iullat, iullon, iurlat, iurlon, ilrlat, ilrlon;
        int    illlat, illlon, ivertmer, istndlt, iseclat, ier;
        int    i, mynum;

/*---------------------------------------------------------------------*/

/*
 *	Initialize output variables.
 */
	*iret  = -2;
	*kxsize = IMISSD;
	*kysize = IMISSD;

        for ( i=0; i < 4; i++ ) {
           ilat[i] = IMISSD;
           ilon[i] = IMISSD;
        }
        for ( i=0; i < 3; i++ ) 
           iangles[i] = IMISSD;
/*
 *	Open the AWIPS table file.
 */
	strcpy ( tblnam, "awsiz.tbl" );
	strcpy ( dirsym, "pgen" );
	ftbl = cfl_tbop ( tblnam, dirsym, &ier );
	if  ( ier != 0 )  {
	    *iret = -1;
	    return;
	}

	found = G_FALSE;
	while ( ( fgets ( buffer, 132, ftbl ) != NULL ) && ( ! found ) )
	{
	    if  ( buffer[0] != '!' )  {
		if  ( sscanf ( buffer, "%d%d%d%d%d%d%d%d%d%d%d%d%d%d",
			       &mynum, &ikxsize, &ikysize, &iullat, 
			       &iullon, &iurlat, &iurlon, &ilrlat, 
			       &ilrlon, &illlat, &illlon, &ivertmer, 
			       &istndlt, &iseclat ) >= 14 )  {
		     
		    if  ( pick == mynum )  {
			found  = G_TRUE;
			*kxsize = ikxsize;
			*kysize = ikysize;
			ilat[0] = iullat;
			ilon[0] = iullon;
			ilat[1] = iurlat;
			ilon[1] = iurlon;
			ilat[2] = ilrlat;
			ilon[2] = ilrlon;
			ilat[3] = illlat;
			ilon[3] = illlon;
			iangles[0] = ivertmer;
			iangles[1] = istndlt;
			iangles[2] = iseclat;
			*iret    = 0;
		    }
		}
	    }
	}
	cfl_clos ( ftbl, &ier );
}
