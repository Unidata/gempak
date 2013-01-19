#include "geminc.h"
#include "gemprm.h"
#include "ghcmn.h"

void gh_gclr ( char *tag, int *idef, int *icolr, int *ired, 
				int *igreen, int *iblue, int *iret )
/************************************************************************
 * gh_gclr								*
 *									*
 * This subroutine sets the color for GPTPC subroutines.		*
 *									*
 * gh_gclr ( tag, idef, icolr, ired, igreen, iblue, iret )  		*
 *									*
 * Input parameters:							*
 *	*tag		char		Tag for color number		*
 *	*idef		int		Default color number		*
 *									*
 * Output parameters:							*
 *	*icolr		int		Returned GEMPAK color number	*
 *	*ired		int		Red color number		*
 *	*igreen		int		Green color number		*
 *	*iblue		int		Blue color number		*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		 6/01   					*
 * A. Hardy/GSC		 7/01 	Added '=' to ired check			*
 ***********************************************************************/
{
    int		ier, ier1, lens;
    char	def[3], data[25], dred[25], dgreen[25], dblue[25], 
                tag1[33], tag2[33]; 
/*--------------------------------------------------------------------*/
     *iret = 0;

   /*
    *  Retrieve the tag color data.
    */

     cst_inch ( *idef, def, &ier);
     cst_rmbl (tag, tag1, &lens, &ier);
     cst_gtag ( tag1, table, def, data, iret );
     cst_numb ( data, icolr, &ier1 );

     cst_rmbl( tag1,  tag2, &lens, &ier);
     strcat ( tag2, "_red" );
     cst_gtag ( tag2, table, def, dred, &ier );

    /*
     *  If the tag was found, see if there are red, green, blue
     *  values that correspond with that tag.
     */

     cst_numb ( dred, ired, &ier );
     if ( (*ired >= 0 ) && (  *ired != *idef ) ) {

         *iret = 2;

	 cst_rmbl( tag,  tag2, &lens, &ier);
         strcat ( tag2, "_green" );
         cst_gtag ( tag2, table, def, dgreen, &ier );
	 cst_numb ( dgreen, igreen, &ier );

	 cst_rmbl( tag,  tag2, &lens, &ier);
         strcat ( tag2, "_blue" );
         cst_gtag ( tag2, table, def, dblue, &ier1 );
	 cst_numb ( dblue, iblue, &ier );
     }
     else  {
         *ired   = -1;
         *igreen = -1;
         *iblue  = -1;
     }
}
