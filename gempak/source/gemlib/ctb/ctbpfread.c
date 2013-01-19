#include "geminc.h"
#include "gemprm.h"

#define CTB_PREFERENCE
#include "ctbcmn.h"


void ctb_pfread ( int *iret )
/************************************************************************
 * ctb_pfread								*
 *									*
 * This function reads the info. from preference table and initializes  *
 * the _prefTbl structure.		                                *
 *									*
 * ctb_pfread ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *                                       -1 = no entry in the table     *
 *					 -3 = unable to load PREFS_TBL  *
 *				  	 -4 = unable to allocate memory *
 **									*
 * Log:									*
 * H. Zeng/EAI          08/02   initial coding                          *
 * H. Zeng/EAI          08/02   changed grpnam to prfnam                *
 * T. Lee/SAIC		11/03	increased MAX_PREF_VAL			*
 ***********************************************************************/
{
int  ii, nr, ier, ier2;
char buffer[256], str[128], prfnam[128]; 
FILE *fp;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  Read the preference table and add entries into the 
     *  prefs[] array. 
     */
    nr = 0;
    fp = cfl_tbop( PREFS_TBL, "config", &ier );
    if ( ier == 0 ) { 
	cfl_tbnr( fp, &nr, &ier);
    }
	    
    if ( nr == 0 ) {
	fclose(fp);
        *iret = -1;
        return;
    }

    /*
     *  Allocate space for the groups.
     */
    _prefTbl.prefs = (pref_ent_t *)malloc( (size_t)nr * sizeof(pref_ent_t) );
    if (_prefTbl.prefs == NULL ) {
	*iret = -4;
	return;
    }
  
    /*
     * Read preference table entries into _prefTbl structure.
     */
    ii = 0;
    cfl_trln(fp, 256, buffer, &ier);
    while ( ier >= 0 && ier != 4 ) {

        if ( ier == 0 ) {

                /*
                 * Read info. from buffer.
                 */
                prfnam[0] = '\0';
                str[0]    = '\0';
                sscanf(buffer, "%s %s", prfnam, str);
                cst_lcuc(prfnam, prfnam, &ier2);
            
	        cst_ncpy( _prefTbl.prefs[ii].tag, prfnam, 
						  MAX_PREF_STR, &ier2);
	        cst_ncpy( _prefTbl.prefs[ii].val, str, 
						  MAX_PREF_VAL, &ier2); 
                ii++;
                           
        } /* the end of if ( ier == 0 ) */

	cfl_trln(fp, 256, buffer, &ier);

    } /* End of while */    
            
    _prefTbl.npref = ii;
    fclose(fp);
  
}

/*=====================================================================*/
