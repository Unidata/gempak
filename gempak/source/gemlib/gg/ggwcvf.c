#include "geminc.h"
#include "gemprm.h"
#include "proto_vf.h"

void gg_wcvf ( int *inumb, int *iret )
/************************************************************************
 * GG_WCVF        							*
 *        								*
 * This subroutine creates the cancel SAW and SEL text products. The    *
 * input file for the creation for the SEL and SAW product is the 	*
 * original watch text file, ww####.txt. A search path for this file	*
 * can be set up in the table file woudef.tbl.				*
 *        								*
 * GG_WCVF ( INUMB, IRET )						*
 *        								*
 * Input parameters:        						*
 *        								*
 * Output parameters:        						*
 *        IRET		INTEGER		Return code			*
 *        								*
 **        								*
 * Log:        								*
 * A. Hardy/NCEP         7/03						*
 * A. Hardy/NCEP         9/03		Added 'include proto_vf.h' 	*
 * M. Li/SAIC		10/04		Replace ctb_rdwou with ctb_rdprf*
 ***********************************************************************/
{
	
    int		lens, ier;
    char 	fname[256], path[120], tag[25], flnme[256];
    char	tblnam[72], dirsym[160];
/*--------------------------------------------------------------------*/
       *iret   = 0;
       flnme[0] = '\0';

      /*
       * Create watch file name.
       */

       sprintf ( fname, "ww%04d.txt", *inumb);

      /*
       * Find the file name.
       */

       strcpy (tag, "WOU_SRCH");
       strcpy ( tblnam, "woudef.tbl" );
       strcpy ( dirsym, "txtprd" );
       ctb_rdprf ( tblnam, dirsym, tag, path, &ier );

       fl_fpth ( path, fname, flnme, &ier, strlen(path), strlen(fname),
                 sizeof(flnme) );

       st_null ( flnme, fname, &lens, &ier, sizeof(flnme), sizeof(fname) );
       
      /*
       * Read in watch information into structure.
       */

       vfgttxt ( fname, &ier );

      /*
       * Create cancel messages for SAW and SEl.
       */

       if ( ier == 0 ) {
           vfcnsaw  ( &ier );
           vfcnsel  ( &ier );
       }
       else {
	 *iret = ier;
       }
}
