/************************************************************************
 * nwx_dstruct.h                                                        *
 *                                                                      *
 * Header file containing the data container structures.                *
 *									*
 * There are two levels in the data representation: group and products, *
 * i.e. the data products are organized into groups. Global variable    *
 * guid_grp_head points to the head of structure guid_grp link list and *
 * the total number of the groups is saved in the global variable 	*
 * guid_ngrp.								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin            6/95        					*
 * S. Jacobs/NCEP	11/97	Fixed comment for guid_ngrp		*
 * E. Safford/SAIC	12/07 	renamed to nwx_dstruct.h		*
 ***********************************************************************/
#ifndef _nwx_dstruct_H_
#define _nwx_dstruct_H_

#define MAX_GRP_NAME 		30
#define MAX_PROD_NAME 		30
#define MAX_KEY			30 

/*
 * GUI data representation for each product.
 */
struct guid_prod {
   char         prdname[MAX_PROD_NAME];         /* product name in GUI */
   char         key[MAX_KEY];                   /* search key in master table */
   int     	index;                          /* index to master table */
};

/*
 * GUI data representation for each group.
 */
struct guid_grp {
   char              grpname[MAX_GRP_NAME];     /* group name in GUI */
   struct guid_prod *prod;			/* array of products */
   int     	     nprod;                     /* # of products */
   struct guid_grp  *nextgrp;                   /* next group    */
};

extern struct guid_grp *guid_grp_head;          /* global variable */
                                                /* base pointer to */
                                                /* GUI group structure */

extern int  guid_ngrp;				/* global variable */
						/* total number of groups */ 

#undef MAX_GRP_NAME 
#undef MAX_PROD_NAME
#undef MAX_KEY	

#endif /* _guid_H_ */
