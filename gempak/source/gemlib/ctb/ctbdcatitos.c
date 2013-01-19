#include "geminc.h"
#include "gemprm.h"

void ctb_dcatitos ( int *catnum, char *catgry, int *iret )
/************************************************************************
 * ctb_dcatitos								*
 *									*
 * This function translates a category type number into a category name.*
 *									*
 * ctb_dcatitos ( catnum, catgry, iret )				*
 *									*
 * Input parameters:							*
 *	*catnum		int		Category number			*
 *									*
 * Output parameters:							*
 *	*catgry		char		Category name 			*
 *	*iret		int		Return code			*
 *				  	0 - Normal			*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 * M. Li/SAIC		03/08	Added CAT_ENS				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    /* 
     *  Translate the type number into the category names in "gemprm.h".
     */
    switch ( *catnum ) { 
        	
	case CAT_IMG:
	    strcpy( catgry, "CAT_IMG" );
          break;
        	
	case CAT_SFC:
	    strcpy( catgry, "CAT_SFC" );	     
          break;
        
	case CAT_SFF:
	    strcpy( catgry, "CAT_SFF" );	     
          break;

	case CAT_SND:
	    strcpy( catgry, "CAT_SND" );	     
          break;

	case CAT_SNF:
	    strcpy( catgry, "CAT_SNF" );	     
          break;

	case CAT_GRD:
	    strcpy( catgry, "CAT_GRD" );	     
          break;

	case CAT_VGF:
	    strcpy( catgry, "CAT_VGF" );	     
          break;

	case CAT_MSC:
	    strcpy( catgry, "CAT_MSC" );	     
          break;

	case CAT_ENS:
            strcpy( catgry, "CAT_ENS" );
          break;

        default:
	    strcpy( catgry, "CAT_NIL" );
          break;
	  
    }

}

