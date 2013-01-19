/************************************************************************
 * cesgtcmn.h  								*
 *									*
 * Contains the structure for group type settings for elements.		*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	02/02	move grp structs from cescmn.h		*
 ***********************************************************************/
#ifndef CESGTCMN_H
#define CESGTCMN_H


#define MAX_GRP_STR	( 20 )


/*
 *  Group type table types and definitions.
 */
typedef struct {
    char	name[20];		/* group type name */
    char	type;			/* group type ID   */
} mstr_grp_t;

typedef struct {
    char	name[20];		/* group type name */
    char	type;			/* group type ID   */
    int		nitems;			/* Number of label items  */
    char	info[GRP_MXELE][GRP_MXINFO];/* attribute info. of label item */
    char	label[GRP_MXELE][GRP_MXCHR];/* label items     */
} avail_grp_t;



/*
 *  Note there are two sub structures contained in the grpTbl_t:
 *
 *    The mstr_grp_t contains the master list of all allowable group type
 *    names and their associated group type id numbers.  This list is hard
 *    coded.
 *
 *    The avail_grp_t struct contains the available group types.  The contents
 *    of this structure will be a subset of the contents of mstr_grp_t.  It is
 *    loaded from the grptyp.tbl file.  If an entry in that file matches
 *    one of the entries in the mstr_grp_t, then it is written to the
 *    avail_grp_t struct.  The pgen group GUIs, and label and attribute
 *    information that is associated with particular group types are stored
 *    in this struct.
 */
typedef struct {
    int		nmstr;		/* total # of group types */
    mstr_grp_t	*mstr;		/* master list of group types and id #s */
    int		ngrp;		/* total # of available group types  */
    avail_grp_t	*grps;		/* available groups types & assoc. info */ 
} grpTbl_t;



#ifdef CES_GRPTYP
	grpTbl_t		_grpTbl;
#else
	extern grpTbl_t	_grpTbl;
#endif


#endif  /* CESGTCMN_H */
