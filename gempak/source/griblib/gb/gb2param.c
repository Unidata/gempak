#include "gb2def.h"

void gb2_param ( char *wmovartbl, char *lclvartbl, int prmext, Gribmsg *cmsg, 
                char *param, int *scal, float *msng, int *iret )
/************************************************************************
 * gb2_param								*
 *									*
 * This routine gets the parameter values                               *
 * from the GRIB2 PDS, and obtains the GEMPAK parameter information     *
 * from the appropriate GRIB2 parameter table.                          *
 *									*
 * If either wmovartbl or lclvartbl are NULL, the default tables are    *
 * read.                                                                *
 *									*
 * gb2_param ( wmovartbl, lclvartbl, cmsg, prmext, param, scal, msng,   *
 *             iret )							*
 *									*
 * Input parameters:							*
 *      *wmovartbl      char            WMO parameter table             *
 *      *lclvartbl      char            Local parameter table           *
 *      prmext          int             Flag to add ens info to param   *
 *	*cmsg  	    struct Gribmsg      GRIB2  message structure        *
 *									*
 * Output parameters:							*
 *      *param          char            12 character parameter name.    *
 *	*scal		int		scale factor associated with    *
 *                                      current parameter               *
 *	*msng		float		missing value associated with   *
 *                                      current parameter               *
 *	*iret		int		return code			*
 *                                          1 = No gempak param name    *
 *                                              defined for this grid   *
 **									*
 * Log:									*
 * S. Gilbert/NCEP      12/04                                           *
 * S. Gilbert/NCEP      10/05	Fix null character location     	*
 * S. Gilbert/NCEP      10/05	Use new routines to read tables 	*
 * S. Gilbert/NCEP      11/07	Added prmext arg			*
 * S. Jacobs/NCEP	 7/08	Add append of FE and AE for error grids	*
 * S. Jacobs/NCEP	 2/11	Replaced strncpy with cst_ncpy		*
 ***********************************************************************/
{
    int     ret, ier, disc, cat, id, pdtn, iver, lclver, len, ier2;
    char    blanks[13]="            ", ctemp[20];
    G2Vinfo  g2var;
    G2vars_t  *g2vartbl;

/*---------------------------------------------------------------------*/

    *iret = 0;
    cst_ncpy ( param, "UNKNOWN", 12, &ier2);

    /* 
     *  Get Parameter into from Paramter table(s).
     */
    iver=cmsg->gfld->idsect[2];
    lclver=cmsg->gfld->idsect[3];
    disc=cmsg->gfld->discipline;
    cat=cmsg->gfld->ipdtmpl[0];
    id=cmsg->gfld->ipdtmpl[1];
    pdtn=cmsg->gfld->ipdtnum;

    if ( iver == 255 ) {      /* use only local parameter table  */
        gb2_gtlclvartbl( lclvartbl, cmsg->origcntr, lclver, &g2vartbl, 
                         &ier);
        if ( ier == 0 ) {
           /* 
            *  Find paramter info in Local Parameter table.
            */
            gb2_skvar( disc, cat, id, pdtn, g2vartbl, &g2var, &ier);
        }
    }
    else {
        if ( ( disc < 192   || disc == 255   ) && 
             ( cat  < 192   || cat  == 255   ) &&
             ( id   < 192   || id   == 255   ) &&
             ( pdtn < 32768 || pdtn == 65535 ) ) {
           /* 
            *  Get WMO Parameter table.
            */
            gb2_gtwmovartbl ( wmovartbl, iver, &g2vartbl, &ier);
            if ( ier == 0 ) {
               /* 
                *  Find paramter info in WMO Parameter table.
                */
                gb2_skvar( disc, cat, id, pdtn, g2vartbl, &g2var, &ier);
            }
        }
        else {
           /* 
            *  Get Local Parameter table.
            */
            gb2_gtlclvartbl( lclvartbl, cmsg->origcntr, lclver, &g2vartbl, 
                             &ier);
            if ( ier == 0 ) {
               /* 
                *  Find paramter info in Local Parameter table.
                */
                gb2_skvar( disc, cat, id, pdtn, g2vartbl, &g2var, &ier);
            }
        }
    }
    if ( ier != 0 ) {
        if ( ier == -32 ) {
           sprintf( ctemp,"%d|%d|%d|%d", disc, cat, id, pdtn);
           er_wmsg("GB",&ier,ctemp,&ret,2,strlen(ctemp));
        }
        *iret=1;
        return;
    }

    /* 
     *  Insert time range period in parameter abbreviation,
     *  if necessary.
     */
    gb2_ctim ( cmsg->tmrange, g2var.gemname );

    /* 
     *  Adjust ensemble information in parameter abbreviation,
     *  if necessary.
     */
     if ( prmext != 0 ) {
       gb2_ens ( cmsg->gfld, g2var.gemname );
     }

    /* 
     *  Adjust probability information in parameter abbreviation,
     *  if necessary.
     */
    gb2_prob ( cmsg->gfld, g2var.gemname );

    /* 
     *  Adjust statistical information in parameter abbreviation,
     *  if necessary.
     */
    gb2_stat ( cmsg->gfld, g2var.gemname );

    /*
     *  Add forecast or analysis error identifier, if necessary.
     */
    if ( cmsg->gfld->ipdtmpl[2] == 6 ) strcat ( g2var.gemname, "FE" );
    if ( cmsg->gfld->ipdtmpl[2] == 7 ) strcat ( g2var.gemname, "AE" );

    len = strlen(g2var.gemname);
    cst_ncpy ( param, g2var.gemname, 12, &ier2);
    if ( len > 12 ) param[12]='\0';
    if ( len < 12 )  {     /*  pad gempak parameter name with blanks  */
       strncat( param, blanks, 12-len );
    }
    *scal = g2var.scale;
    *msng = g2var.missing;

}
