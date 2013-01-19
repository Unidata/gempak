#include "geminc.h"
#include "gemprm.h"

void ctb_dscatitos ( int *scatnum, char *scatgry, int *iret )
/************************************************************************
 * ctb_dscatitos							*
 *									*
 * This function translates a sub-type number into a sub-category name.	*
 *									*
 * ctb_dscatitos ( scatnum, scatgry, iret )				*
 *									*
 * Input parameters:							*
 *	*scatnum	int		Sub-category type number	*
 *									*
 * Output parameters:							*
 *	*scatgry	char		Sub-category name 		*
 *	*iret		int		Return code			*
 *				  	0 - Normal			*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    /* 
     *  Translate the number into the category names in "gemprm.h".
     */
    switch ( *scatnum ) { 
        	
	case SCAT_SFC:
	    strcpy( scatgry, "SCAT_SFC" );
          break;
        	
	case SCAT_SHP:
	    strcpy( scatgry, "SCAT_SHP" );	     
          break;
        
	case SCAT_SFF:
	    strcpy( scatgry, "SCAT_SFF" );	     
          break;

	case SCAT_FFG:
	    strcpy( scatgry, "SCAT_FFG" );	     
          break;

	case SCAT_SND:
	    strcpy( scatgry, "SCAT_SND" );	     
          break;

	case SCAT_SNF:
	    strcpy( scatgry, "SCAT_SNF" );	     
          break;

	case SCAT_FCT:
	    strcpy( scatgry, "SCAT_FCT" );	     
          break;

	case SCAT_ANL:
	    strcpy( scatgry, "SCAT_ANL" );	     
          break;

        default:
	    strcpy( scatgry, "SCAT_NIL" );
          break;
	  
    }

}

