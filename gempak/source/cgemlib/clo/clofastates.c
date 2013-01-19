#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

#define DEFAULT_STRING  "-"
#define CSTL_WTRS       "CSTL_WTRS"

void clo_fastates ( char *faarea, char *strin, char sep, char *ptype, 
                    char *strout, int *iret )
/************************************************************************
 * clo_fastates		                                                *
 *                                                                      *
 * clo_fastates ( faarea, strin, sep, ptype, strout, iret)		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *faarea         char            Name of FA area			*
 *      *strin          char            String containing the list of 	*
 *					state, Great Lake and CSTL_WTRS *
 *					abbreviations			*
 *       sep 		char		Separator			*
 *	*ptype		char		Product type (AIRMET, SIGMET)	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *strout         char            String containg the correctly 	*
 *					ordered list of abbreviations	*
 *      *iret           int             Return code                     *
 *                                      = 1 - some strin entries ignored*
 *                                      = 0 - normal                   	*
 *                                      =-1 - unable to open bnds table	*
 *                                      =-2 - unable to find faarea	*
 *                                      =-3 - unable to sort strin	*
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 10/04   Created                                 *
 * m.gamazaychikov/SAIC 11/04   Fixed a problem with the return code    *
 * B. Yin/SAIC		11/04   Fixed a problem with the last char      *
 ***********************************************************************/
{
int     founda, founds, iarea, istmax, ii, ier, nbnds, num, len, which;
int     ipos, ilen, icoast, iskip, maxnum, ier2, ier3, maxch;
char    fnm[132],bndnam[14], states[128], info[128], tag[12];
char    **stinarr, **stoutarr, sepr[2];
FILE    *fp;

/*---------------------------------------------------------------------*/

    /*
     *  Convert input strings to upper case.
     */
     cst_lcuc ( faarea, faarea, &ier );
     cst_lcuc ( strin, strin, &ier );
     cst_lcuc ( ptype, ptype, &ier );

    /*
     * Intialize the constants
     */
     num    = 50;
     len    = 10;
     *iret  = 0;
     maxch  = 12;
     maxnum = 50;
     icoast = FALSE;
     founda = FALSE;
     founds = FALSE;
     iskip  = FALSE;
     sprintf (sepr, "%c", sep);
     strcpy ( strout, DEFAULT_STRING );
     strcpy ( tag, "STATES" );
     strcpy ( bndnam, "FA_AREA_BNDS" );
     which = clo_which ( bndnam );

    /*
     *  Open boundary file.
     */
     strcpy(fnm, clo.loc[which].bnd.filename);
     fp = (FILE *)cfl_tbop(fnm, "bounds", &ier);

     if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        strcpy ( strout, "\0" );
        return;
     }

    /*
     * Allocate memory for array size, initialize arrays
     */
     G_MALLOC ( stinarr,  char*, num, "CLO_FASTATES - stinarr" );
     G_MALLOC ( stoutarr, char*, num, "CLO_FASTATES - stinarr" );
     for ( ii = 0; ii < num; ii++ ) {
        G_MALLOC ( stinarr [ii], char, len+1, "CLO_FASTATES - stinarr"  );
        G_MALLOC ( stoutarr[ii], char, len+1, "CLO_FASTATES - stoutarr" );
     }

     for ( ii = 0; ii < num; ii++ ) {
         strcpy(stoutarr[ii], DEFAULT_STRING) ;
     }

    /* 
     * Extract states from the correct bounds area 
     */
     nbnds = clo.loc[which].bnd.nbnd;
     ii = 0;
     while ( ( ii < nbnds ) && ( !founda ) ) {
        cst_ptmt ( faarea, clo.loc[which].bnd.bound[ii].name, &founda, &ier);
        ii++;
     }

     if ( !founda ) { 
        *iret = -2; 
        strcpy ( strout, "\0" );
     }
     else {
        iarea = ii-1;

        strcpy (info, clo.loc[which].bnd.bound[iarea].info);

        cst_gtag ( tag, info, DEFAULT_STRING, states, &ier); 

       /* 
        * Find out if the states and input strings contain CSTL_WTRS string
        */
        cst_srch (0,(int)strlen(states), CSTL_WTRS, states, &ipos, &ier2);
        cst_srch (0,(int)strlen(strin),  CSTL_WTRS, strin,  &ipos, &ier3);
        if ( ier2 == 0 && ier3 == 0 ) icoast = TRUE;
        if ( ier2 != 0 && ier3 == 0 ) iskip  = TRUE;

       /* 
        * Copy the input string into an array of string for sorting
        */
        cst_clst (strin, sep, " ", maxnum, maxch, stinarr, &istmax, &ier);

       /* 
        * Sort states in strin to list them in the correct order in strout
        */
        for ( ii = 0; ii < istmax; ii++ ) {
           if ( strcmp( stinarr [ii], CSTL_WTRS ) != 0 ) {
              cst_srch (0,(int)strlen(states), stinarr [ii], states, &ipos, &ier);
              if ( ier == 0 ) {
                 strcpy (stoutarr [ipos/3], stinarr [ii]);
              }
              else {
                 iskip = TRUE ;
              }
           }
        }

       /* 
        * Skip the empty pozitions in the output string array and 
        * construct the output string
        */
        for ( ii = 0; ii < num; ii++ ) {
           if ( ( strcmp( stoutarr [ii], DEFAULT_STRING ) != 0 ) &&
                ( ! founds ) ) {
              strout[0]='\0';
              strcat(strout, stoutarr [ii]);
              strcat(strout, sepr);
              founds = TRUE;
           }
           else if ( ( strcmp( stoutarr [ii], DEFAULT_STRING ) != 0 ) &&
                     ( founds ) ) {
              strcat(strout, stoutarr [ii]);
              strcat(strout, sepr);
           }
        }

       /* 
        * Add CSTL_WTRS string at the end of the output string if necessary
        */
        if ( icoast ) { 
           strcat ( strout, CSTL_WTRS );
        }
        else { 
           cst_lstr ( strout, &ilen, &ier );
           strout [ilen ] = '\0';
        }

       /* 
        * Manage the return codes
        */
        if ( !founds ) {
           *iret = -3;
           strcpy ( strout, "\0" );
        }

        if ( iskip ) {
           *iret = 1;
        }
     }

    /*
     * Free memory and close boundary table file.
     */
     for ( ii = 0; ii < num; ii++ ) {
        G_FREE ( stinarr [ii], char );
        G_FREE ( stoutarr[ii], char );
     }
     G_FREE ( stinarr,  char* );
     G_FREE ( stoutarr, char* );

     cfl_clos ( fp, &ier );
}
