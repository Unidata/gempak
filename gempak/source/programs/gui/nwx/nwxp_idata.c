
#include "nwx_cmn.h" 

/************************************************************************
 * nwxp_idata.c								*
 *                                                                      *
 * This module manages the user interface settings data.  This includes *
 * managing the list of available data located inthe guid_grp_head list *
 * the settings contained in the usrSelect structure, an the auto       *
 * update status.							*
 *                                                                      *
 * CONTENTS:								*
 *	idata_init		initialize data structs			*
 *	idata_getAutoUpdt	get the current auto update setting	*
 *	idata_setAutoUpdt	set the auto update value		*
 ***********************************************************************/


#define TABDIR "nwx"    		/* directory of nwx tables */
#define DATA_TABLE "guidata.tbl"	/* name of the nwx gui data type table */

#define PROD_ARRAY  2		/* initial size of product array */
#define ARRAY_SIZE  80		/* general char string size */

#define TOKENDM	" \t|\n"

#define SIZEOFGRP	sizeof(struct guid_grp)
#define SIZEOFPRD	sizeof(struct guid_prod)


/*
 * Macro function to substitute character c1 with c2
 * in string str
 */
#define STRCSUB(str, c1, c2) { \
	char *p; \
	p = str; \
        for(; *p; p++) \
        	if(*p == c1) *p = c2; \
}
			

struct guid_grp *guid_grp_head; 	/* head of group link list */
int 		guid_ngrp;		/* total # of groups */
G_Boolean	_autoUpdt = G_FALSE;	/* auto update setting */



int idata_init ( int *iret )
/************************************************************************
 * nwxp_idata.c								*
 *                                                                      *
 * int guidInit ( iret )                                                *
 *                                                                      *
 * Create the link list of guid_grp structure and set the global 	*
 * pointer guid_grp_head head of this link list.                  	*
 * returns the total number of groups when successful. 			*
 *									*
 * Input parameters:							*
 *			None						*
 * Output parameters:							*
 *	*iret		int	return code				*
 *				 0 = normal				*
 *				-1 = error reading guidata.tbl		*
 *				-2 = format error in guidata.tbl	*
 * Return:								*
 *			int	number of data groups read		*
 **                                                                     *
 * Log:                                                                 *
 * L. Williams		 6/95						*
 * C. Lin		 6/95						*
 * G. Krueger		 3/96		CFL_SOPN -> CFL_ROPN		*
 * S. Jacobs/NCEP	12/98		Changed fclose to cfl_clos	*
 * T. Piper/SAIC	01/04	Replaced cfl_ropn with cfl_tbop		*
 * E. Safford/SAIC	11/07	use DATA_TABLE, rm filename param	*
 ***********************************************************************/
{
struct	guid_grp  *pgrp; 		/* current group */
struct  guid_prod *pprod;
FILE	*fp;
char	buf[ARRAY_SIZE+1], *ptoken;
int 	ier; 
int     prod_cnt=0, begin_gprod=0;
int	prod_arry_size;

/*---------------------------------------------------------------------*/

    guid_ngrp = 0;			/* total # of groups */
    *iret = G_NORMAL;


    /*
     * open table file
     */
    fp = cfl_tbop(DATA_TABLE, TABDIR, &ier);
    if ((ier != 0) || (fp == NULL)) {
        *iret = -1;
        return( 0 );
    }

    /*
     * initialize the link list and product array size
     */
    guid_grp_head  = pgrp = NULL;
    prod_arry_size = PROD_ARRAY;

    /*
     * read and process table file
     */
    while(fgets(buf, ARRAY_SIZE, fp) != NULL) {

        if ( (ptoken = strtok(buf, TOKENDM)) == NULL ) {
	    continue; 				/* empty line */
        }
	if ( *ptoken == '#' || *ptoken == ';' || *ptoken == '!' ) {
	    continue; 				/* comments */
        }
	switch( ptoken[0] ) {

	    case '{' : 				/* begining of the group */

            /*
             * Locate first product.                 
             * begin_gprod also records the nesty level of pair '{' & '}'.
             */
	        ++begin_gprod;
		break;

	    case '}' : 				/* the end of the group */
	    /*
 	     * Ensure all products are located within the left and
             * right parenthesis.  No nest of '{' is allowed for now. 
             */
	        if (begin_gprod != 1) {
		    *iret = -2;
		    return( 0 );
		}
		else {

		    --begin_gprod;

		    if (prod_cnt > 0) {
		        pgrp->nprod = prod_cnt;
			  
                        /* 
                         * make sure the product array has exact number
                         * of products appearing in the file.
                         */
		         pprod = (struct guid_prod *)realloc(pgrp->prod,
				(size_t)(pgrp->nprod) * SIZEOFPRD);
		         pgrp->prod = pprod;

		    }
		}

		break;

	    default :

                /*
                 * New group
                 */
	        if (begin_gprod == 0) {

		    if ( !pgrp ) { 		/* first group */   
        	        guid_grp_head = pgrp = (struct guid_grp *) 
                                	malloc(SIZEOFGRP);
		    }
		    else {
		        pgrp->nextgrp = (struct guid_grp *) malloc(SIZEOFGRP);

                        /*
                         * move the current group pointer
                         */
		        pgrp = pgrp->nextgrp;
		    }

                    /*
                     * allocate initial product array
                     */
        	    pgrp->prod = (struct guid_prod *) 
				malloc (PROD_ARRAY * SIZEOFPRD);

                    /*
                     *  get group name and convert the '_' to ' '
                     */
		    strcpy(pgrp->grpname, ptoken);
		    STRCSUB(pgrp->grpname, '_', ' ');

		    pgrp->nprod   = 0; 
		    pgrp->nextgrp = NULL; 

		    prod_cnt = 0;
		    prod_arry_size = PROD_ARRAY;

		    ++guid_ngrp;
	       }    

               /*
                * Process products 
                */
	       else if (begin_gprod ==1) {

                   /*
                    * Check if array size needs to increase 
                    */
		   if (prod_cnt >= prod_arry_size) {

		       prod_arry_size += PROD_ARRAY;

		       pprod = (struct guid_prod *)realloc(pgrp->prod,
			 	(size_t)(prod_arry_size) * SIZEOFPRD);
		       pgrp->prod = pprod;

		   }

                   /* 
                    * get product name ( convert '_' -> ' ' )
                    */
		   strcpy(pgrp->prod[prod_cnt].prdname, ptoken); 
		   STRCSUB(pgrp->prod[prod_cnt].prdname, '_', ' ');

                   /*
                    * get product key
                    */
		   ptoken = strtok(NULL, TOKENDM);
		   if ( ptoken && (*ptoken != '*') ){
		       strcpy(pgrp->prod[prod_cnt].key, ptoken);
		       pgrp->prod[prod_cnt].index = -1;
		   }
		   else {
		       strcpy(pgrp->prod[prod_cnt].key, "*");
		       pgrp->prod[prod_cnt].index = -2;
		   }
			
	           prod_cnt++;
	      }

	      break;

        } /* end of switch */

    } /* end of while loop */

    cfl_clos ( fp, &ier );
    fp = NULL;

    return ( guid_ngrp );
}

/*=====================================================================*/

G_Boolean idata_getAutoUpdt ( void )
/************************************************************************
 * idata_getAutoUpdt							*
 *                                                                      *
 * This function returns the current value of the auto update flag    	*
 * (_autoUpdt). 							*
 *                                                                      *
 * G_Boolean idata_getAutoUpdt ( )                                      *
 *                                                                      *
 * Input parameters:							*
 * Output parameters:							*
 *		none							*
 * Return:								*
 *		G_Boolean	state of the auto update flag		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/07	initial coding                   	*
 ***********************************************************************/
{
    return( _autoUpdt );
}

/*=====================================================================*/

void idata_setAutoUpdt ( G_Boolean newValue, int *iret )
/************************************************************************
 * idata_setAutoUpdt							*
 *                                                                      *
 * This function sets the current value of the auto update flag    	*
 * (_autoUpdt) to newValue, provided it is within range.		*
 *                                                                      *
 * void idata_getAutoUpdt ( newValue, iret )                            *
 *                                                                      *
 * Input parameters:							*
 *	G_Boolean	newValue	value for auto update		*
 *									*
 * Output parameters:							*
 * 	int		*iret		 0 = normal			*
 *					-1 = newValue out of range	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/07	initial coding                   	*
 ***********************************************************************/
{

    *iret = 0;

    if( ( newValue != G_TRUE ) && ( newValue != G_FALSE ) ) {
        *iret = -1;
    }
    else {
        _autoUpdt = newValue;
    }

}
