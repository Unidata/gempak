#include "cvgcmn.h"


typedef struct grp_num *numptr;

typedef struct grp_num {
	int		grpnum;
	int		new_grpnum;
	numptr		next_grp;
} grp_num_t;


typedef struct grp_typ *typptr;

typedef struct grp_typ {
	int		grptyp;
	numptr		grps;
	typptr   	next_typ;
	int		number;
} grp_typ_t;


static int		_totalTypes;


/*
 *  private subroutines
 */
static void gmtrx_processGroup (grp_typ_t **head, char grptyp, int grpnum);
static void gmtrx_freeList     (grp_typ_t *head);

/************************************************************************
 * cvggmtrx.c                                                           *
 *                                                                      *
 * This module								*
 *                                                                      *
 * CONTENTS:                                                            *
 *  cvg_gmtrx			process vg file to build grp matrix	*	
 *  gmtrx_processGroup		build a local struct for group info     *
 *  gmtrx_freeList		free the local structs when done	*
 ***********************************************************************/


void cvg_gmtrx ( char *fname, int *numtyps, CVG_mtrx_t **matrix, 
			int *iret )
/************************************************************************
 * cvg_gmtrx								*
 *									*
 * This function reads the given vg file in a single pass and returns a *
 * transformation index which maps the existing group numbers for each  *
 * type with a compressed group number.  The compressed group numbers   *
 * start at 1 and go through n, where n is the number of distinct group *
 * numbers for every group type.					*
 *									*
 * NOTE:  Memory is allocated for **matrix.  When the calling routine   *
 * 	  is finished with **matrix, it must free the memory.  Call     *
 *	  cvg_ggfrmtrx() to accomplish this.				*
 *									*
 * cvg_gmtrx ( fname, numtyps, matrix, iret )				*
 *									*
 * Input parameters:							*
 *	*fname		char            VG file name			*
 *									*
 * Output parameters:							*
 *	*numtyps	int		number of group types in file	*
 *	**matrix	CVG_mtrx_t	transformation matrix		*
 *	*iret		int		Return code			*
 *					  -1  = bad file name		*
 *					  -25 = missing/bad file hdr	*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/02	initial coding               		*
 * E. Safford/SAIC	04/02	correct func name to match file		*
 * E. Safford/SAIC	04/02	improve check on file size     		*
 * T. Lee/SAIC		05/02	closed file				*
 * E. Safford/SAIC	05/02	move file close call			*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    FILE	*fp = NULL;
    char	newfil[FILE_FULLSZ];
    int		curpos, flag, gctr, ier, ier1;

    long	fsize;
    typptr	head_typ = NULL, cur_typ;
    numptr      cur_num;

    VG_DBStruct el;
    CVG_mtrx_t	*mptr;
/*---------------------------------------------------------------------*/

    _totalTypes = 0;
    *iret       = 0;

    cfl_inqr ( fname, NULL, &fsize, newfil, &ier );
    if ( (size_t)fsize < (sizeof(FileHeadType) + sizeof(VG_HdrStruct)) ) {
        *iret = -25;
        return;
    }
    else if ( ier >= 0 ) { 
        fp = cfl_ropn ( fname, NULL, &ier ); 
    }

    if ( ier < 0 || fp == NULL ) {
	*iret = -1;
	return;
    }

 
    curpos = 0; 

    /*
     *  Read the file and build a list of the contents.  Each type node
     *  points to a list of group number nodes when finished.
     */
    while ( (long)curpos < fsize ) {

        cvg_rdhdr ( fname, fp, curpos, (int)fsize, &el, &flag, &ier );
	if ( ier < 0 ) {
	    *iret = ier;
       	    cfl_clos  ( fp, &ier1 ); 
	    return;
	}

	if ( el.hdr.grptyp > 0 && el.hdr.grpnum > 0 ) {
	    gmtrx_processGroup ( &head_typ, el.hdr.grptyp, el.hdr.grpnum );
	}

	curpos += el.hdr.recsz;
    }
    cfl_clos  ( fp, &ier1 ); 


    /*
     *  Allocate the required space in the matrix and load the 
     *  group type/number transformation information.
     */
    if ( head_typ != NULL ) {

	*matrix = ( CVG_mtrx_t *) malloc( _totalTypes * sizeof(CVG_mtrx_t) );

	mptr = *matrix;

        cur_typ = head_typ;

        while ( cur_typ != NULL ) {

	    mptr->grptyp  = cur_typ->grptyp;
	    mptr->numgrps = cur_typ->number;

	    /*
	     *  Allocate the needed space for the in and out values.
	     */
	    mptr->grpin   = (int *) malloc (cur_typ->number * sizeof(int) ); 
	    mptr->grpout  = (int *) malloc (cur_typ->number * sizeof(int) ); 

	    cur_num  = cur_typ->grps;
	    gctr = 0;

	    while ( cur_num != NULL ) {
  		mptr->grpin[gctr]  = cur_num->grpnum; 
  		mptr->grpout[gctr] = cur_num->new_grpnum; 

		cur_num  = cur_num->next_grp;
		gctr++;
	    }

	    cur_typ  = cur_typ->next_typ;
	    mptr++;	
        }

    }


    *numtyps = _totalTypes;

    /*
     *  Clean up
     */
    gmtrx_freeList ( head_typ );

}

/*=====================================================================*/

static void gmtrx_processGroup ( grp_typ_t **head, char grptyp, int grpnum )
/************************************************************************
 * gmtrx_processGroup							*
 *									*
 * This function searches for the group type and group number in the 	*
 * existing list structures.  If the type or number is not found, a new *
 * structure is allocated for these, and the information is stored      *
 * within it.								*
 *									*
 * gmtrx_processGroup ( **head, grptyp, grpnum )			*
 *									*
 * Input parameters:							*
 *	**head		grp_typ_t       pointer to the head of the list *
 *	grptyp		char		group type			*
 *	grpnum		int		group number			*
 *									*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 *  E. Safford/SAIC	04/02	initial coding               		*
 *  E. Safford/SAIC	04/02	correct func name              		*
 ***********************************************************************/

{
    typptr	newtyp, cur_typ, prev_typ;
    numptr 	newnum, cur_num, prev_num;

    Boolean	found_typ, found_grp;
/*---------------------------------------------------------------------*/


    /*
     *  Search for the given group type.  If not found, create a new 
     *  type node.
     */
    found_typ = False;
    cur_typ   = prev_typ = *head;

    while ( cur_typ != NULL && ! found_typ ) {

	if (cur_typ->grptyp == grptyp) {
	    found_typ = True;
	}
	else {
	    prev_typ = cur_typ;
	    cur_typ = cur_typ->next_typ;
	}
    } 

    /*
     *  If found_typ is true, then cur_typ now points to the correct 
     *  group typ.  If False, then the group type does not exist, so
     *  create a new grptyp node.
     */
    if ( !found_typ ) {

	_totalTypes++;

	newtyp = (typptr) malloc( sizeof(grp_typ_t) );
	newtyp->grptyp     = grptyp;
	newtyp->grps       = NULL;
	newtyp->next_typ   = NULL;
	newtyp->number     = 0;

	/*
	 *  If *head is NULL, then point it to the newtyp.  Otherwise, link
	 *  prev_typ->next_typ to newtyp.
	 */
	if ( *head == NULL ) {
	    *head = newtyp;
	}
	else {
	    prev_typ->next_typ = newtyp;
	}

	cur_typ = newtyp; 
    }

    /*
     *  Now search for a match on the group number.  If not found
     *  create a new group node.
     */

    found_grp = False;
    cur_num   = prev_num  = cur_typ->grps;


    while ( cur_num != NULL && !found_grp ) {

	if (cur_num->grpnum == grpnum) {
	    found_grp = True;
	}
	else {
	    prev_num = cur_num;
	    cur_num = cur_num->next_grp;
	}
    }

    if ( !found_grp ) {
	
	newnum = (numptr) malloc( sizeof(grp_num_t) );
	newnum->grpnum     = grpnum;

	cur_typ->number++;
	newnum->new_grpnum = cur_typ->number;

	newnum->next_grp   = NULL;

  	if ( prev_num == NULL ) {
	    cur_typ->grps  = newnum;
	}
	else { 
	    prev_num->next_grp = newnum;
  	} 
	
    }

}


/*=====================================================================*/

static void gmtrx_freeList (grp_typ_t *head)
/************************************************************************
 * gmtrx_freeList							*
 *									*
 * This function frees the allocated memory associated with the list 	*
 * of typptr and numptr structures.                                 	*
 *									*
 * gmtrx_freeList ( *head)						*
 *									*
 * Input parameters:							*
 *	*head		grp_typ_t       pointer to the head of the list *
 *									*
 * Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 *  E. Safford/SAIC	04/02	initial coding               		*
 *  E. Safford/SAIC	04/02	correct func name              		*
 ***********************************************************************/
{
    numptr	cur_num, prev_num;
    typptr 	cur_typ, prev_typ;

/*---------------------------------------------------------------------*/

    
    /*
     *  Free the list by removing the group list from each type node, 
     *  then removing the type node itself. 
     */
    if ( head != NULL ) {
    
        cur_typ = head;
        while ( cur_typ != NULL ) {

	    cur_num  = cur_typ->grps;
	    while ( cur_num != NULL ) {

		prev_num = cur_num;
		cur_num  = cur_num->next_grp;

		free (prev_num);
	    }

	    prev_typ = cur_typ;
	    cur_typ  = cur_typ->next_typ;

	    free (prev_typ);
	
        }

    }

}
