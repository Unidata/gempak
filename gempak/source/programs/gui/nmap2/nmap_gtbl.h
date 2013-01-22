/************************************************************************
 * nmap_gtbl.h								*
 *									*
 * Header file for the grid table structures.				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	02/96						*
 * G. Grosshans/SPC	11/97	Increased GTBL_NGRP from 10 to 25	*
 * G. Grosshans/SPC	 1/99	Increased GTBL_NGRP from 25 to 50	*
 * G. Grosshans/SPC	 2/99	Increased GTBL_NPRD from 50 to 75	*
 * S. Jacobs/NCEP	 9/99	Increased GTBL_NGRD from 30 to 60	*
 * S. Law/GSC		11/99	changed all model references to grid	*
 * S. Jacobs/NCEP	 3/01	Increased GTBL_NGRD from 60 to 120	*
 * A. Hardy/NCEP	12/04	Increased GTBL_NPRD from 75 to 150	*
 * T. Piper/SAIC	10/05	Removed GTBL_NGRD, GTBL_NGRP, and	*
 *				GTBL_NPRD; dynamically allocating	*
 * M. Li/SAIC		02/08	Add modgrp_t & gtbl_readEnsSel		*
 * M. Li/SAIC		05/08	Add selModLst to modgrp_t		*
 ***********************************************************************/

typedef struct {
	char	*grpnam;		/* group name			*/
	int	nprods;			/* number of products		*/
	int     *iprod;			/* ptr into mtbinfo struct	*/
}mgrp_t;

typedef struct {
	char	*grdnam;		/* grid name			*/
	int	ngroups;		/* number of groups		*/
	mgrp_t	**groups;		/* group structure		*/
}mgrd_t;

typedef struct {
	int	ngrids;			/* number of grids		*/
	mgrd_t	**grids;		/* grids structure		*/
}gridtbl_t;

typedef struct {
	int	nprods;
	char	**prdnam;		/* product name			*/
	char	**prdpth;		/* path name			*/
}mprodinfo_t;

typedef struct {
    int  numgrp;
    char **modgrp;			/* model group name		*/
    char **modlst;			/* model list string		*/
    char **selModLst;			/* model list string		*/
}modgrp_t;

void 	gtbl_init (	char		*fn,
			gridtbl_t	*gtbl,
			int		*iret );
int	gtbl_cmpGroup ( mgrp_t		**group1,
			mgrp_t		**group2 );
int	gtbl_cmpGrid (	mgrd_t		**grid1,
			mgrd_t		**grid2 );
void    gtbl_readEnsTbl (  char    	*fn, 
			   modgrp_t	*enstbl1,
                           gridtbl_t	*enstbl2,
                           int     	*iret);
