#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "nmap_gtbl.h"  


#define	TRUE		1
#define	FALSE		0
#define	GRID_SEP	";"

struct	_prod {
	int	indx;
	char	name[40];
};

int	gtbl_cmpProd ( 	struct _prod	*prod1,
			struct _prod	*prod2 );

mprodinfo_t      _grdProdInfo, _ensProdInfo;

/************************************************************************
 * nmap_gtbl.c                                                          *
 *                                                                      *
 * This module reads a grid restore file table into an nmap_gtbl	*
 * structure given the structure and table name.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   gtbl_init()	process grid table file into grid structure	*
 *   gtbl_readEnsTbl()	process model group table grid structures	*
 *   gtbl_getProd() 	return product name and path			*
 *   gtbl_getEnsProd() 	return product name and path for ensemble	*
 *									*
 *   gtbl_cmpProd()	fxn to compare product structure		*
 *   gtbl_cmpGroup()	fxn to compare group structure			*
 *   gtbl_cmpGrid()	fxn to compare grid structure			*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void gtbl_init ( char *fn, gridtbl_t *gtbl, int *iret) 
/************************************************************************
 * gtbl_init								*
 *                                                                      *
 * This function reads a grid restore file table into an gridtbl_t	*
 * structure given the structure and table name.  It also saves the	*
 * product information in _grdProdInfo structure.			*
 *									*
 * void gtbl_init(fn, gtbl, iret)					*
 *									* 
 * Input parameters:							*
 *	*fn		char		table filename			*
 * Output parameters:							*
 *	*gtbl		gridtbl_t	grid table structure		*
 * Return parameters:                                                   *
 *	*iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	02/97						*
 * S. Law/GSC		11/99	changed all model references to grid	*
 * S. Jacobs/NCEP	 3/01	Added call to ngd_gnms			*
 * A. Hardy/NCEP	12/04	Increased string size for record and grd*
 * T. Piper/SAIC	10/05	Dynamically allocate grids, groups, and	*
 *				iprod 					*
 * m.gamazaychikov/SAIC 01/06   Changed templt string length to MXTMPL 	*
 ***********************************************************************/
{
char	grd[256], grid[32], group[32],path[256],prod[40],templt[MXTMPL];
char	*ptr, record[512];
FILE	*ftbl;
int	igrid, jgroup, kprod, im, jm, km, ier, nent, ni, nn, nnm;
int	indx, ipos, one=1;
nmlst_t	namarr[MXNMFL];

/*---------------------------------------------------------------------*/

    *iret = 0;
    ftbl = cfl_tbop( fn, "nmap", iret);
    if ( ftbl == NULL ) {
	return;
    }

    nent = 0;
    while ( !feof(ftbl) ) {

        /*
         * read a record
         */
	cfl_trln(ftbl, 512, record, &ier);

	if ( ier == 0 )  nent++;

    }

    if ( nent != 0 )  {
	_grdProdInfo.nprods = nent;
	G_MALLOC(_grdProdInfo.prdnam, char *, nent, "_grdProdInfo.prdnam");
	G_MALLOC(_grdProdInfo.prdpth, char *, nent, "_grdProdInfo.prdpth");
    }
    else {
	fclose ( ftbl );
	return;
    }

    rewind ( ftbl );

    nn = 0;
    gtbl->ngrids = 0;
    gtbl->grids = NULL;

    while ( nn < nent ) {

        /*
         * read a record
         */
	cfl_trln(ftbl, 512, record, &ier);

	if ( ier == 0 ) {

	    sscanf(record, "%s %s %s %s", prod, path, group, grd);

	    G_MALLOC(_grdProdInfo.prdnam[nn], char, (int)strlen(prod)+1, "_grdProdInfo.prdnam[nn]");
	    G_MALLOC(_grdProdInfo.prdpth[nn], char, (int)strlen(path)+1, "_grdProdInfo.prdpth[nn]");

	    strcpy ( _grdProdInfo.prdnam[nn], prod );
	    strcpy ( _grdProdInfo.prdpth[nn], path );

	    ptr = strtok(grd, GRID_SEP);

	    /*
	     * foreach grid (model) listed in record, add this product and
	     * group to table structure.
	     */
	    while ( ptr ) {

		strcpy(grid, ptr);

		/*
		 * Check this grid for named storm/volcano.
		 */

		/*
		 * Note: Even though ngd_gnms will check for an
		 *       appropriate template, this method saves
		 *       a LOT of time at the startup of nmap2.
		 */
		ctb_dttmpl ( grid, templt, &ier );
		cst_nocc ( templt, '*', 1, 0, &ipos, &ier );

		if  ( ier == 0 )  {
		    ngd_gnms ( grid, &nnm, namarr, &ier );
		}
		else {
		    nnm = 0;
		}

		ni = 0;
		do {
		    if  ( nnm > 0 )  {
			cst_nocc ( grid, ':', 1, 0, &ipos, &ier );
			if  ( ier == 0 )  {
			    grid[ipos] = CHNULL;
			}
			strcat ( grid, ":" );
			strcat ( grid, namarr[ni] );
		    }

		/*
		 * for this grid (model), get index into table structure.
		 * if not there already, add to table.
		 * Skip this entry if too many grids.
		 */
		    im = gtbl->ngrids;
		    if ( im > 0 ) {
			for ( igrid = 0 ; igrid < gtbl->ngrids ; igrid++ ) {
			    if ( strcmp(gtbl->grids[igrid]->grdnam, grid) == 0 ) {
				im = igrid;
				break;
			    }
			}
		    }
		    if ( im == gtbl->ngrids )  {
			gtbl->ngrids++;
			G_REALLOC(gtbl->grids, mgrd_t*, gtbl->ngrids, "gtbl->grids");
			G_MALLOC (gtbl->grids[im], mgrd_t, one, "gtbl->grids[im]");
			G_MALLOC (gtbl->grids[im]->grdnam, char, (int)strlen(grid)+1, "gtbl->grids[im]->grdnam");
			strcpy(gtbl->grids[im]->grdnam, grid);
			gtbl->grids[im]->ngroups = 0;
			gtbl->grids[im]->groups = NULL;
		    }

		/*
		 * for this group, get index into table structure.
		 * if not there already, add to table.
		 * Skip this entry if too many groups.
		 */
		    jm = gtbl->grids[im]->ngroups;
		    if ( jm > 0 ) {
			for ( jgroup = 0 ; jgroup < gtbl->grids[im]->ngroups ; jgroup++ ) {
			    if ( strcmp(gtbl->grids[im]->groups[jgroup]->grpnam, group) == 0 ) {
				jm = jgroup;
			    break;
			    }
			}
		    }
		    if ( jm == gtbl->grids[im]->ngroups )  {
			gtbl->grids[im]->ngroups++;
			G_REALLOC(gtbl->grids[im]->groups, mgrp_t*, gtbl->grids[im]->ngroups, "gtbl->grids[im]->groups");
			G_MALLOC (gtbl->grids[im]->groups[jm], mgrp_t, one, "gtbl->grids[im]->groups[jm]");
			G_MALLOC (gtbl->grids[im]->groups[jm]->grpnam, char, (int)strlen(group)+1, "gtbl->grids[im]->groups[jm]->grpnam");
			strcpy(gtbl->grids[im]->groups[jm]->grpnam, group);
			gtbl->grids[im]->groups[jm]->nprods = 0;
			gtbl->grids[im]->groups[jm]->iprod = NULL;
		    }

		/*
		 * for this path/product, get index into table structure.
		 * if not there already, add to table.
		 * Skip this entry if too many products.
		 */
		    km = gtbl->grids[im]->groups[jm]->nprods;
		    if ( km > 0 ) {
		        for (kprod = 0; kprod < gtbl->grids[im]->groups[jm]->nprods; kprod++){
			    indx = gtbl->grids[im]->groups[jm]->iprod[kprod];
			    if (strcmp(_grdProdInfo.prdnam[indx], prod) == 0 ) {
			        km = kprod;
			        break;
			    }
		        }
		    }
		    if ( km == gtbl->grids[im]->groups[jm]->nprods ) {
			gtbl->grids[im]->groups[jm]->nprods++;
                        G_REALLOC (gtbl->grids[im]->groups[jm]->iprod, int, gtbl->grids[im]->groups[jm]->nprods, "gtbl->grids[im]->groups[jm]->iprod");
			gtbl->grids[im]->groups[jm]->iprod[km] = nn;
		    }
	    	    ni++;
		} while ( ni < nnm );
	/*
	 *  Get next grid name, if any
	 */
		ptr = strtok(NULL, GRID_SEP);
	    }  /*  while ( ptr )  */
	}  /*  if ( ier == 0 )  */
        nn++;
    }  /*  while ( nn < nent )  */ 

    fclose ( ftbl );


    for ( igrid = 0 ; igrid < gtbl->ngrids ; igrid++ ) {
	for ( jgroup = 0; jgroup < gtbl->grids[igrid]->ngroups ; jgroup++ ) {
	    /*
	     *  Sort product names alphabetically
	     */
	    gtbl_sortProd( gtbl->grids[igrid]->groups[jgroup]->nprods,
			gtbl->grids[igrid]->groups[jgroup]->iprod );

	}

	/*
	 *  Sort group names alphabetically
	 */
	qsort(gtbl->grids[igrid]->groups, gtbl->grids[igrid]->ngroups,
	    sizeof(mgrp_t *), (int(*)(const void*, const void*))gtbl_cmpGroup );

    }

    /*
     *  Sort grid names alphabetically
     */
    qsort(gtbl->grids, gtbl->ngrids, sizeof(mgrd_t *),
		(int(*)(const void*, const void*))gtbl_cmpGrid);

    return;

}

/*=====================================================================*/

int gtbl_getProd ( int indx, char **prdname, char **prdpath ) 
/************************************************************************
 * gtbl_getProd								*
 *                                                                      *
 * This function gets the pointers to product name and product path 	*
 * based on the index.  The pointer should never be freed or changed.	*
 * If a change is needed, then copies of the product name and path      *
 * should be made.							*
 *									*
 * int gtbl_getProd(indx, prdname, prdpath)				*
 * 									*
 * Input parameters:							*
 * indx		int		index of the product			*
 *									*
 * Output parameters:							*
 * **prdname	char	pointer to a product name			*
 * **prdpath	char	pointer to a product name			*
 *									*
 * Return parameters:                                                   *
 * gtbl_getProd	int		0 = successful 				*
 * 			       -1 = invalid index 			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * C. Lin/EAI		03/97						*
 * S. Law/GSC		11/99	changed all model references to grid	*
 ***********************************************************************/
{

	if ( indx < 0 || indx >= _grdProdInfo.nprods) {
		prdname = NULL;
		prdpath = NULL;
		return (-1);
	}
	else {
		*prdname = _grdProdInfo.prdnam[indx];
		*prdpath = _grdProdInfo.prdpth[indx];
		return (0);
	}
}

/*=====================================================================*/

void gtbl_sortProd ( int nprods, int iprod[] )
/************************************************************************
 * gtbl_sortProd                                                     	*
 *                                                                      *
 * This function sorts product names.					*
 *                                                                      *
 * void gtbl_sortProd(nprods, iprod)					*
 *                                                                      *
 * Input parameters:                                                    *
 * nprods	int		# of products - both input and output	*
 * iprod[]	int		product index array			*
 *                                                                      *
 * Return parameters:                                                   *
 * 			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	02/97						*
 * S. Law/GSC		11/99	changed all model references to grid	*
 * M. Li/GSC		02/00 	return if nprods < 2			*
 ***********************************************************************/
{
struct	_prod	*prd;
int	ii;
/*---------------------------------------------------------------------*/

	if ( nprods < 2 ) return;

	G_MALLOC(prd, struct _prod, nprods, "gtbl_sortProd:  prd");

	for ( ii = 0; ii < nprods; ii++ ) {
		prd[ii].indx = iprod[ii];		
		strcpy(prd[ii].name, _grdProdInfo.prdnam[iprod[ii]] );
	}

        qsort(prd, nprods, sizeof(struct _prod), 
		(int(*)(const void*, const void*))gtbl_cmpProd );
	
	for ( ii = 0; ii < nprods; ii++ ) {
		iprod[ii] = prd[ii].indx;
	}

	G_FREE ( prd, struct _prod );

}

/*=====================================================================*/

int gtbl_cmpProd ( struct _prod *prod1, struct _prod *prod2 )
/************************************************************************
 * gtbl_cmpProd	                                                     	*
 *                                                                      *
 * This function compares product names in the mprd_t structure.	*
 *                                                                      *
 * int gtbl_cmpProd(prod1, prod2)					*
 *                                                                      *
 * Input parameters:                                                    *
 * *prod1	struct _prod	struct _prod element			*
 * *prod2	struct _prod	struct _prod element			*
 *                                                                      *
 * Return parameters:                                                   *
 * gtbl_cmpProd	int		Return code (ala strcmp)		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	02/97						*
 * S. Law/GSC		11/99	changed all model references to grid	*
 ***********************************************************************/
{
	return (strcmp(prod1->name, prod2->name));
}

/*=====================================================================*/

int gtbl_cmpGroup ( mgrp_t **group1, mgrp_t **group2 )
/************************************************************************
 * gtbl_cmpGroup                                                     	*
 *                                                                      *
 * This function compares group names in the mgrp_t structure.		*
 *                                                                      *
 * int gtbl_cmpGroup(group1, group2)					*
 *                                                                      *
 * Input parameters:                                                    *
 * **group1	mgrp_t		mgrp_t element				*
 * **group2	mgrp_t		mgrp_t element				*
 *                                                                      *
 * Return parameters:                                                   *
 * gtbl_cmpGroup	int	Return code (ala strcmp)		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	02/97						*
 * S. Law/GSC		11/99	changed all model references to grid	*
 ***********************************************************************/
{
	return (strcmp((*group1)->grpnam, (*group2)->grpnam));
}

/*=====================================================================*/

int gtbl_cmpGrid ( mgrd_t **grid1, mgrd_t **grid2 )
/************************************************************************
 * gtbl_cmpGrid                                                     	*
 *                                                                      *
 * This function compares grid names in the mgrd_t structure.		*
 *                                                                      *
 * int gtbl_cmpGrid(grid1, grid2)					*
 *                                                                      *
 * Input parameters:                                                    *
 * **grid1	mgrd_t		mgrd_t element				*
 * **grid2	mgrd_t		mgrd_t element				*
 *                                                                      *
 * Return parameters:                                                   *
 * gtbl_cmpGrid	int		Return code (ala strcmp)		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	02/97						*
 * S. Law/GSC		11/99	changed all model references to grid	*
 ***********************************************************************/
{
	return (strcmp((*grid1)->grdnam, (*grid2)->grdnam));
}

/*=====================================================================*/

void gtbl_readEnsTbl ( char *fn, modgrp_t *enstbl1, gridtbl_t *enstbl2, int *iret) 
/************************************************************************
 * gtbl_readEnsTbl							*
 *                                                                      *
 * This function reads a model group and restore file table into an 	*
 * modgrp_t and gridtbl_t structures given the structure and table name.*
 * It also saves the product information.				*
 *									*
 * void gtbl_readEnsTbl(fn, enstbl1, enstbl2, iret)			*
 *									* 
 * Input parameters:							*
 *	*fn		char		table filename			*
 * Output parameters:							*
 *	*enstbl1	modgrp_t	model list structure		*
 *	*enstbl2	gridtbl_t	model group product structre	*
 * Return parameters:                                                   *
 *	*iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		02/08						*
 * M. Li/SAIC           05/08   Add selModLst to modgrp_t               *
 ***********************************************************************/
{
char	grd[256], grid[32], group[32],path[256],prod[40];
char	*ptr, record[512], mod_grp[80], mod_list[LLMXLN], ignore[80];
FILE	*ftbl;
int	igrid, jgroup, kprod, im, jm, km, ier, nent, nent1, nent2, nn;
int	indx, one=1, nn1, nn2;

/*---------------------------------------------------------------------*/

    *iret = 0;
    ftbl = cfl_tbop( fn, "nmap", iret);
    if ( ftbl == NULL ) {
	return;
    }

    nent  = 0;
    nent1 = 0;
    while ( !feof(ftbl) ) {

        /*
         * read a record
         */
	cfl_trln(ftbl, 512, record, &ier);

	if ( ier == 0 ) {
	    if ( strncmp(record,"DEFINE",6) == 0 ) nent1++;
	    nent++;
	}

    }

    nent2 = nent - nent1;

    if ( nent >= 2 )  {
	_ensProdInfo.nprods = nent2;
	G_MALLOC(_ensProdInfo.prdnam, char *, nent2, "_ensProdInfo.prdnam");
	G_MALLOC(_ensProdInfo.prdpth, char *, nent2, "_ensProdInfo.prdpth");

	enstbl1->numgrp = nent1;
	G_MALLOC(enstbl1->modgrp, char *, nent1, "enstbl1->modgrp");
	G_MALLOC(enstbl1->modlst, char *, nent1, "enstbl1->modlst");
	G_MALLOC(enstbl1->selModLst, char *, nent1, "enstbl1->selModLst");
    }
    else {
	fclose ( ftbl );
	return;
    }

    rewind ( ftbl );

    nn = 0;
    nn1 = 0;
    nn2 = 0;
    enstbl2->ngrids = 0;
    enstbl2->grids = NULL;

    while ( nn < nent ) {

        /*
         * read a record
         */
	cfl_trln(ftbl, 512, record, &ier);

	if ( ier == 0 ) {

	    /*
	     * Read first part of table.
	     */
	    if ( strncmp(record,"DEFINE",6) == 0 ) {
		sscanf(record, "%s %s %s", ignore, mod_grp, mod_list); 
		G_MALLOC(enstbl1->modgrp[nn1], char, (int)strlen(mod_grp)+1, "enstbl1->modgrp");
		G_MALLOC(enstbl1->modlst[nn1], char, (int)strlen(mod_list)+1, "enstbl1->modlst");
		G_MALLOC(enstbl1->selModLst[nn1], char, 300, "enstbl1->selModLst");

		strcpy ( enstbl1->modgrp[nn1], mod_grp );
		strcpy ( enstbl1->modlst[nn1], mod_list );
		strcpy ( enstbl1->selModLst[nn1], mod_list );
		nn1++;
	    }
	    else {
	    	/*
	     	* Read second part of table.
	     	*/
	     	sscanf(record, "%s %s %s %s %s", ignore, prod, path, group, grd);

	    	G_MALLOC(_ensProdInfo.prdnam[nn2], char, (int)strlen(prod)+1, "_ensProdInfo.prdnam[nn2]");
	    	G_MALLOC(_ensProdInfo.prdpth[nn2], char, (int)strlen(path)+1, "_ensProdInfo.prdpth[nn2]");

	    	strcpy ( _ensProdInfo.prdnam[nn2], prod );
	    	strcpy ( _ensProdInfo.prdpth[nn2], path );

	    	ptr = strtok(grd, GRID_SEP);

	       /*
	     	* foreach grid (model) listed in record, add this product and
	     	* group to table structure.
	     	*/
	    	while ( ptr ) {

		    strcpy(grid, ptr);

	       	    /*
		     * for this grid (model), get index into table structure.
		     * if not there already, add to table.
		     * Skip this entry if too many grids.
		     */
    		    im = enstbl2->ngrids;
    		    if ( im > 0 ) {
    			for ( igrid = 0 ; igrid < enstbl2->ngrids ; igrid++ ) {
    			    if ( strcmp(enstbl2->grids[igrid]->grdnam, grid) == 0 ) {
    				im = igrid;
    				break;
    			    }
    			}
    		    }
    		    if  ( im == enstbl2->ngrids )  {
    			enstbl2->ngrids++;
    			G_REALLOC(enstbl2->grids, mgrd_t*, enstbl2->ngrids, "enstbl2->grids");
    			G_MALLOC (enstbl2->grids[im], mgrd_t, one, "enstbl2->grids[im]");
    			G_MALLOC (enstbl2->grids[im]->grdnam, char, (int)strlen(grid)+1, "enstbl2->grids[im]->grdnam");
    			strcpy(enstbl2->grids[im]->grdnam, grid);
    			enstbl2->grids[im]->ngroups = 0;
    			enstbl2->grids[im]->groups = NULL;
    		    }

    		   /*
    		    * for this group, get index into table structure.
    		    * if not there already, add to table.
    		    * Skip this entry if too many groups.
    		    */
    		    jm = enstbl2->grids[im]->ngroups;
    		    if ( jm > 0 ) {
    			for ( jgroup = 0 ; jgroup < enstbl2->grids[im]->ngroups ; jgroup++ ) {
    			    if ( strcmp(enstbl2->grids[im]->groups[jgroup]->grpnam, group) == 0 ) {
    				jm = jgroup;
    				break;
    			    }
    			}
    		    }
    		    if ( jm == enstbl2->grids[im]->ngroups )  {
    			enstbl2->grids[im]->ngroups++;
    			G_REALLOC(enstbl2->grids[im]->groups, mgrp_t*, enstbl2->grids[im]->ngroups, "enstbl2->grids[im]->groups");
    			G_MALLOC (enstbl2->grids[im]->groups[jm], mgrp_t, one, "enstbl2->grids[im]->groups[jm]");
    			G_MALLOC (enstbl2->grids[im]->groups[jm]->grpnam, char, (int)strlen(group)+1, "enstbl2->grids[im]->groups[jm]->grpnam");
    			strcpy(enstbl2->grids[im]->groups[jm]->grpnam, group);
    			enstbl2->grids[im]->groups[jm]->nprods = 0;
    			enstbl2->grids[im]->groups[jm]->iprod = NULL;
    		    }

    		/*
    		 * for this path/product, get index into table structure.
    		 * if not there already, add to table.
    		 * Skip this entry if too many products.
    		 */
    		    km = enstbl2->grids[im]->groups[jm]->nprods;
    		    if ( km > 0 ) {
    			for (kprod = 0; kprod < enstbl2->grids[im]->groups[jm]->nprods; kprod++){
    			    indx = enstbl2->grids[im]->groups[jm]->iprod[kprod];
    			    if (strcmp(_ensProdInfo.prdnam[indx], prod) == 0 ) {
    				km = kprod;
    				break;
    			    }
    			}
    		    }
    		    if ( km == enstbl2->grids[im]->groups[jm]->nprods ) {
    			enstbl2->grids[im]->groups[jm]->nprods++;
			G_REALLOC (enstbl2->grids[im]->groups[jm]->iprod, int, enstbl2->grids[im]->groups[jm]->nprods, "enstbl2->grids[im]->groups[jm]->iprod");
    			enstbl2->grids[im]->groups[jm]->iprod[km] = nn2;
    		    }
		/*
	 	 *  Get next grid name, if any
	 	 */
		    ptr = strtok(NULL, GRID_SEP);
	    	}  /*  while ( ptr )  */
		nn2++;
	    }  /*  else    */
	} /*  if ( ier == 0 ) */
        nn++;
    }  /*  while ( nn < nent )  */ 

    fclose ( ftbl );


    for ( igrid = 0 ; igrid < enstbl2->ngrids ; igrid++ ) {
	for ( jgroup = 0; jgroup < enstbl2->grids[igrid]->ngroups ; jgroup++ ) {
	    /*
	     *  Sort product names alphabetically
	     */
	    gtbl_sortEnsProd( enstbl2->grids[igrid]->groups[jgroup]->nprods,
			enstbl2->grids[igrid]->groups[jgroup]->iprod );

	}

	/*
	 *  Sort group names alphabetically
	 */
	qsort(enstbl2->grids[igrid]->groups, enstbl2->grids[igrid]->ngroups,
	    sizeof(mgrp_t *), (int(*)(const void*, const void*))gtbl_cmpGroup );

    }

    /*
     *  Sort grid names alphabetically
     */
    qsort(enstbl2->grids, enstbl2->ngrids, sizeof(mgrd_t *),
		(int(*)(const void*, const void*))gtbl_cmpGrid);

    return;

}

/*=====================================================================*/

int gtbl_getEnsProd ( int indx, char **prdname, char **prdpath ) 
/************************************************************************
 * gtbl_getProd								*
 *                                                                      *
 * This function gets the pointers to product name and product path 	*
 * based on the index.  The pointer should never be freed or changed.	*
 * If a change is needed, then copies of the product name and path      *
 * should be made.							*
 *									*
 * int gtbl_getProd(indx, prdname, prdpath)				*
 * 									*
 * Input parameters:							*
 * indx		int		index of the product			*
 *									*
 * Output parameters:							*
 * **prdname	char	pointer to a product name			*
 * **prdpath	char	pointer to a product name			*
 *									*
 * Return parameters:                                                   *
 * gtbl_getEnsProd	int	0 = successful 				*
 * 			       -1 = invalid index 			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * M. Li/SAIC		02/08 	Modified from gtbl_getProd		*
 ***********************************************************************/
{

	if ( indx < 0 || indx >= _ensProdInfo.nprods) {
		prdname = NULL;
		prdpath = NULL;
		return (-1);
	}
	else {
		*prdname = _ensProdInfo.prdnam[indx];
		*prdpath = _ensProdInfo.prdpth[indx];
		return (0);
	}
}

/*=====================================================================*/

void gtbl_sortEnsProd ( int nprods, int iprod[] )
/************************************************************************
 * gtbl_sortEnsProd                                                     *
 *                                                                      *
 * This function sorts product names.					*
 *                                                                      *
 * void gtbl_sortEnsProd(nprods, iprod)					*
 *                                                                      *
 * Input parameters:                                                    *
 * nprods	int		# of products - both input and output	*
 * iprod[]	int		product index array			*
 *                                                                      *
 * Return parameters:                                                   *
 * 			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		02/08 	Modified from gtbl_sortProd		*
 ***********************************************************************/
{
struct	_prod	*prd;
int	ii;
/*---------------------------------------------------------------------*/

	if ( nprods < 2 ) return;

	G_MALLOC(prd, struct _prod, nprods, "gtbl_sortEnsProd:  prd");

	for ( ii = 0; ii < nprods; ii++ ) {
		prd[ii].indx = iprod[ii];		
		strcpy(prd[ii].name, _ensProdInfo.prdnam[iprod[ii]] );
	}

        qsort(prd, nprods, sizeof(struct _prod), 
		(int(*)(const void*, const void*))gtbl_cmpProd );
	
	for ( ii = 0; ii < nprods; ii++ ) {
		iprod[ii] = prd[ii].indx;
	}

	G_FREE ( prd, struct _prod );

}

/*=====================================================================*/
