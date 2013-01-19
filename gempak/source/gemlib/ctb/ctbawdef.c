#include "geminc.h"
#include "gemprm.h"

void ctb_awdef ( char *pil, char *awpid, char *kloc, char *flcls, 
		 char *prodid, char *extnd, char *wmo, char *bckgnd,
		 int *rettim, int *kmap, int *crdflg, int *sclint, 
		 int *sclfac, int *areacd, int *label, int *cordfg, 
		 int *nrefpt, int *ktime, int *pick, int *iret )           
/************************************************************************
 * ctb_awdef								*
 *									*
 * This subroutine reads the AWIPS product definition tables for the 	*
 * requested product.							*
 *									*
 * ctb_awdef ( pil, awpid, kloc, flcls, prodid, extnd, wmo, bckgnd,     *
 *             rettim, kmap, crdflg, sclint, sclfac, areacd, label,     *
 *             cordfg,  nrefpt, ktime, pick, iret )                     *
 *									*
 * Input parameters:							*
 *	*pil		char		AWIPS PIL			*
 *									*
 * Output parameters:							*
 *      *awpid          char            AWIPS ID number                 *
 *      *kloc           char            Originating organization ID     *
 *      *flcls          char            Field classification            *
 *      *prodid         char            Graphic product ID              *
 *      *extnd          char            Product ID extension            *
 *      *wmo            char            WMO header                      *
 *      *bckgnd         char            Map background name             *
 *      *rettim         int             Retention time                  *
 *	*kmap		int		Projection indicator            *
 *      *crdflg         int             Graphic definition coord. flag  *
 *      *sclint         int             Scale integer                   *
 *      *sclfac         int             Scale fraction                  *
 *      *areacd         int             Area code                       *
 *      *label          int             Label code                      *
 *      *cordfg         int             Map background def. coord. flag *
 *      *nrefpt         int             Number of reference points      *
 *      *ktime          int             Valid hour                      *
 *      *pick           int             Map coordinate flag             *
 *	*iret		int		Return code			*
 *					  -1 = table cannot be opened	*
 *					  -2 = entry not found		*
 **									*
 * Log:									*
 * A. Hardy/GSC	 	9/98	Copied from CTB_AFOS			*
 ***********************************************************************/
{

	FILE   *ftbl;
	char   tblnam[72], dirsym[72], buffer[133], tpil[4];
        char   tawpid[11], tloc[5], tflcls[2], tprodid[11], 
	       textnd[7], twmo[7], tbckgnd[7]; 
	int    found, imap, irettim, icrdflg, isclint, isclfac; 
	int    iareacd, ilabel, icordfg, inrefpt, iktime, ipick; 
        int    i, ier;

/*---------------------------------------------------------------------*/

/*
 *	Initialize output variables.
 */
	*iret     = -2;
        *rettim   = IMISSD;
	*kmap     = IMISSD;
	*crdflg   = IMISSD;
	*sclint   = IMISSD;
	*sclfac   = IMISSD;
	*areacd   = IMISSD;
	*label    = IMISSD;
	*cordfg   = IMISSD;
	*nrefpt   = IMISSD;
	*ktime    = IMISSD;
	*pick     = IMISSD;
	awpid[0]  = CHNULL;
	kloc[0]   = CHNULL;
	flcls[0]  = CHNULL;
	prodid[0] = CHNULL;
	extnd[0]  = CHNULL;
	wmo[0]    = CHNULL;
	bckgnd[0] = CHNULL;

/*
 *	Open the awips table file.
 */
	strcpy ( tblnam, "awdef.tbl" );
	strcpy ( dirsym, "pgen" );
	ftbl = cfl_tbop ( tblnam, dirsym, &ier );
	if  ( ier != 0 )  {
	    *iret = -1;
	    return;
	}

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	found = G_FALSE;
	while ( ( fgets ( buffer, 132, ftbl ) != NULL ) && ( ! found ) )
	{
	    if  ( buffer[0] != '!' )  {
		if  ( sscanf ( buffer,"%4s%10s%4s%1s%d%10s%6s%6s%6s%d%d%d%d%d%d%d%d%d%d",
			       &tpil[0], &tawpid[0], &tloc[0], 
			       &tflcls[0], &irettim, &tprodid[0], 
			       &textnd[0], &twmo[0], &tbckgnd[0], 
			       &imap, &icrdflg, &isclint, &isclfac, 
			       &iareacd, &ilabel, &icordfg, &inrefpt, 
			       &iktime, &ipick) >= 19 )  {
		    i = strcmp ( tpil,  pil );
		    if  ( i == 0 )  {
			found = G_TRUE;
			cst_ncpy ( awpid, tawpid, 10, &ier );
			cst_ncpy ( kloc, tloc, 4, &ier );
			cst_ncpy ( flcls, tflcls, 1, &ier );
			cst_ncpy ( prodid, tprodid, 10, &ier );
			cst_ncpy ( extnd, textnd, 6, &ier );
			cst_ncpy ( wmo, twmo, 6, &ier );
			cst_ncpy ( bckgnd, tbckgnd, 6, &ier );
			*rettim  = irettim;
			*kmap    = imap;
			*crdflg  = icrdflg; 
			*sclint  = isclint; 
			*sclfac  = isclfac;
			*areacd  = iareacd; 
			*label   = ilabel;    
			*cordfg  = icordfg;
		        *nrefpt  = inrefpt; 
			*ktime   = iktime;
			*pick    = ipick;
			*iret    = 0;
		    }
		}
	    }
	}

	cfl_clos ( ftbl, &ier );
}
