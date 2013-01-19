#include "geminc.h"
#include "gemprm.h"

void wbc_dcon ( char *wtype, int len1, int len2, char *constr, 
                char *perstr, int *iret )
/************************************************************************
 * wbc_dcon								*
 *                                                                      *
 * This function creates the watch conditions explanation paragraph for *
 * severe thunderstorms and tornadoes. This information is used by the 	*
 * SPC's PWN and SEL text products.					*
 *                                                                      *
 * wbc_dcon ( wtype, len1, len2, constr, perstr, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*wtype		char		Watch type			*
 *	len1		int		Max length of constr		*
 *	len2		int		Max length of perstr		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*constr		char		CONDITIONS ARE FAVORABLE string *
 *	*perstr		char		PERSONS IN THESE AREAS string	*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP         5/03						*
 * A. Hardy/NCEP	10/03		Increased ncpy length 51->52	*
 * T. Piper/SAIC	02/04	Removed unused variable hold		*
 ***********************************************************************/
{
    int     len, ier;
    char    tmpstr[500];
/*-------------------------------------------------------------------*/
    *iret = 0;
    tmpstr[0] = '\0'; 
    constr[0] = '\0';  
    perstr[0] = '\0'; 

   /*
    * Create 'CONDITIONS ARE FAVORABLE' string.
    */

    tmpstr[0] = '\0'; 
    if ( strcmp(wtype,"SEVERE THUNDERSTORM") == 0 ){
       sprintf ( tmpstr, "A %s WATCH MEANS CONDITIONS ARE", wtype);
       cst_ncat ( tmpstr, " FAVORABLE FOR SEVERE THUNDERSTORMS", &len, &ier);
    }

    if ( strcmp(wtype,"TORNADO") == 0 ){
        sprintf ( tmpstr, "A %s WATCH MEANS CONDITIONS ARE", wtype);
        cst_ncat ( tmpstr, " FAVORABLE FOR TORNADOES AND SEVERE THUNDERSTORMS", 
	           &len, &ier);
    }
    cst_ncat ( tmpstr, " IN AND CLOSE TO THE WATCH AREA.", &len, &ier);

    len = G_MIN ( len1, (int)strlen(tmpstr) );
    cst_ncpy ( constr, tmpstr, len, &ier );

   /*
    * Create 'PERSONS IN THESE AREAS' sstring.
    */

    
    len = 200;
    cst_ncpy ( perstr, " PERSONS IN THESE AREAS SHOULD BE ON THE LOOKOUT FOR", 
               52, &ier);
    cst_ncat ( perstr, " THREATENING WEATHER CONDITIONS AND LISTEN FOR LATER",
               &len, &ier);
    cst_ncat ( perstr, " STATEMENTS AND POSSIBLE WARNINGS.", &len, &ier);

}
