#include "geminc.h"
#include "gemprm.h"
#include "cesgtcmn.h"


#define  NUM_GROUPS 	( 39 )


void ces_gtlmstr ( int *iret )
/************************************************************************
 * ces_gtlmstr								*
 *									*
 * This function loads the hard-coded master list of group types and    *
 * their associated group id numbers into the _grpTbl.mstr[] array.	*
 *									*
 * It should ONLY be called from ces_gtrtbl!				*
 *									*
 * Note that there are additional group types which are used for        *
 * internal grouping only, and are NEVER presented to the user.  These  *
 * are numbered from 100 downward, and are defined in the pgprm.h file.	*
 *									*	
 * ces_gtlmstr ( iret )							*
 * 									*
 * Input parameters:							*
 *			None						*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				 -4 = unable to allocate required space *
 *									*
 **									*
 * E. Safford/SAIC	02/02	initial coding				*
 * E. Safford/SAIC	05/02	rm CCFP, replace with MESO_DSC		*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * A. Hardy/NCEP	10/03	added TSTMOLK 				*
 * M. Li/SAIC		08/06	Added EXE_SVR and EXE_FIRE		*
 * H. Zeng/SAIC		02/07	Added 7 additional group types		*
 * F. J. Yen/NCEP	04/07	Added ENH_PD1 and ENH_PD2		*
 * G. Grosshans/SPC	02/09	Deleted ENH_PD1 and ENH_PD2, and added	*
 *				ENH20, ENH00, ENH04, ENH12		*
 * G. Grosshans/SPC	02/10	Added ENH16				*
 ***********************************************************************/
{
    int	 	ier;
/*---------------------------------------------------------------------*/
 
    *iret = G_NORMAL;

    /*
     * Construct the master group type list, mapping of all possible 
     * group names and id pairs.   
     * 
     * REMEMBER: When adding a name and number pair (new or replacing a 
     *           DEVn), don't forget to increase NUM_GROUP and update 
     *           documentation in the group type table (grptyp.tbl).
     */

    /*
     *  Avoid memory leak if somehow called more than once.
     */
    if ( _grpTbl.mstr != NULL ) {
	free (_grpTbl.mstr);
    }


    _grpTbl.nmstr = NUM_GROUPS;
    _grpTbl.mstr  = (mstr_grp_t *)malloc( _grpTbl.nmstr * sizeof(mstr_grp_t));

    if ( _grpTbl.mstr == NULL ) {
	*iret = -4;
	return;
    }

    cst_ncpy( _grpTbl.mstr[ 0].name, "UNDEF",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 0].type =  0;

    cst_ncpy( _grpTbl.mstr[ 1].name, "CLOUD",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 1].type =  1;

    cst_ncpy( _grpTbl.mstr[ 2].name, "TURB",    MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 2].type =  2;

    cst_ncpy( _grpTbl.mstr[ 3].name, "FRONT",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 3].type =  3;

    cst_ncpy( _grpTbl.mstr[ 4].name, "JETS",    MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 4].type =  4;

    cst_ncpy(_grpTbl.mstr[ 5].name, "HIGH",     MAX_GRP_STR, &ier  ); 
    _grpTbl.mstr[ 5].type =  5;

    cst_ncpy(_grpTbl.mstr[ 6].name, "LOW",      MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 6].type =  6;

    cst_ncpy(_grpTbl.mstr[ 7].name, "OUTLOOK",  MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 7].type =  7;

    cst_ncpy(_grpTbl.mstr[ 8].name, "LABEL",    MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 8].type =  8;

    cst_ncpy(_grpTbl.mstr[ 9].name, "TROPICL",  MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[ 9].type =  9;

    cst_ncpy(_grpTbl.mstr[10].name, "STNMDL",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[10].type = 10;

    cst_ncpy(_grpTbl.mstr[11].name, "MRFSTN",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[11].type = 11;

    cst_ncpy(_grpTbl.mstr[12].name, "HAILOTLK", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[12].type = 12;

    cst_ncpy(_grpTbl.mstr[13].name, "TORNOTLK", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[13].type = 13;

    cst_ncpy(_grpTbl.mstr[14].name, "WINDOTLK", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[14].type = 14;

    cst_ncpy(_grpTbl.mstr[15].name, "TOTL_SVR", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[15].type = 15;

    cst_ncpy(_grpTbl.mstr[16].name, "FIREOUTL", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[16].type = 16;

    cst_ncpy(_grpTbl.mstr[17].name, "CATG_SVR", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[17].type = 17;

    cst_ncpy(_grpTbl.mstr[18].name, "MESO_DSC", MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[18].type = 18;

    cst_ncpy(_grpTbl.mstr[19].name, "DEV1",     MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[19].type = 19;

    cst_ncpy(_grpTbl.mstr[20].name, "DEV2",     MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[20].type = 20;

    cst_ncpy(_grpTbl.mstr[21].name, "DEV3",     MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[21].type = 21;

    cst_ncpy(_grpTbl.mstr[22].name, "DEV4",     MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[22].type = 22;

    cst_ncpy(_grpTbl.mstr[23].name, "DEV5",     MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[23].type = 23;

    cst_ncpy(_grpTbl.mstr[24].name, "TSTMOLK",  MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[24].type = 24;

    cst_ncpy(_grpTbl.mstr[25].name, "EXT_SVR",  MAX_GRP_STR, &ier );
    _grpTbl.mstr[25].type = 25;

    cst_ncpy(_grpTbl.mstr[26].name, "EXT_FIRE",  MAX_GRP_STR, &ier );
    _grpTbl.mstr[26].type = 26;

    cst_ncpy(_grpTbl.mstr[27].name, "ISOBARS",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[27].type = 27;

    cst_ncpy(_grpTbl.mstr[28].name, "HI_FCST",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[28].type = 28;

    cst_ncpy(_grpTbl.mstr[29].name, "LO_FCST",   MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[29].type = 29;

    cst_ncpy(_grpTbl.mstr[30].name, "WHFT",      MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[30].type = 30;

    cst_ncpy(_grpTbl.mstr[31].name, "WHM",       MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[31].type = 31;

    cst_ncpy(_grpTbl.mstr[32].name, "WPER",      MAX_GRP_STR, &ier ); 
    _grpTbl.mstr[32].type = 32;

    cst_ncpy(_grpTbl.mstr[33].name, "PROB",      MAX_GRP_STR, &ier );
    _grpTbl.mstr[33].type = 33;

    cst_ncpy(_grpTbl.mstr[34].name, "ENH20",   MAX_GRP_STR, &ier );
    _grpTbl.mstr[34].type = 34;

    cst_ncpy(_grpTbl.mstr[35].name, "ENH00",   MAX_GRP_STR, &ier );
    _grpTbl.mstr[35].type = 35;

    cst_ncpy(_grpTbl.mstr[36].name, "ENH04",   MAX_GRP_STR, &ier );
    _grpTbl.mstr[36].type = 36;

    cst_ncpy(_grpTbl.mstr[37].name, "ENH12",   MAX_GRP_STR, &ier );
    _grpTbl.mstr[37].type = 37;

    cst_ncpy(_grpTbl.mstr[38].name, "ENH16",   MAX_GRP_STR, &ier );
    _grpTbl.mstr[38].type = 38;

}
