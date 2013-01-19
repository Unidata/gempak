 /************************************************************************
 * afcmn.h								*
 *									*
 * This file contains structures, defines, and prototypes used by the   *
 * textlib/airmet library routines.					*
 *									*
 **									*
 * Log:									*
 *  E. Safford/SAIC	08/06	initial coding				*
 *  E. Safford/SAIC	09/06	add af_applyAttr()			*
 *  E. Safford/SAIC	09/06	add ROUNDUP macro  			*
 *  E. Safford/SAIC	09/06	add prototype for af_getPolyVerts	*
 *  J. Wu/SAIC		10/06	add REDUCE_INCR_PCT ,REDUCE_INCR_DST	*
 *				af_queryPrefs and modify af_reducePts	*
 *  E. Safford/SAIC	10/06	change FZLVL_THRES from 5 to 7		*
 *  J. Wu/SAIC		10/06	add "int twin" to record the index of	*
 *				twin format structure from area rules	*
 *  J. Wu/SAIC		10/06	add GFA_EPSILON and OUT_EPSILON		*
 *  J. Wu/SAIC		01/07	load extended FA area bounds		*
 *  J. Wu/SAIC		01/07	Default REDUCE_INCR_PCT/DST to 0.0	*
 *  D.W.Plummer/NCEP    03/07   Add REDUCE_INCR_PCT_ORIG                *
 *  B. Yin/SAIC		03/07	Add af_getIssueTm, ad_overrideIssueTm	*
 *				Add day and issuance to af_consWording	*
 * J. Wu/SAIC		03/07   Add af_replaceIntPt & CLUSTER_DIST      *
 * D.W.Plummer/NCEP	03/07	Add dumpGFA_Elem_Format			*
 * J. Wu/SAIC		04/07   Add GFA_S_Attr structure & af_getSSAttr *
 * J. Wu/SAIC		05/07   Store common points for extended FA	*
 *				area bounds      			*
 * E. Safford/SAIC	07/07	add NUM_*_HAZARDS, hazard type arrays	*
 * B. Yin/SAIC		09/07   add af_parseWording			*
 * J. Wu/SAIC		09/07   add af_TURBRenameHILO			*
 ***********************************************************************/
#ifndef AFCMN_H
#define AFCMN_H

#include "geminc.h"
#include "gemprm.h"
#include "vgtag.h"
#include "vgstruct.h"
#include "gpc.h"
#include "proto_gpc.h"

/*
 *  Library definitions
 */
#define	AREA_LIMIT	(3000)		/* In unit of square nautical miles */
#define	NUM_FA_REGION	(3)		/* Number of FA regions */
#define	NUM_FA_AREAS	(6)		/* Number of FA areas */
#define	NUM_TYPES	(3)		/* Number of FA areas */
#define	MAX_BOUND_PT	(100)		/* Max. num. of points for a boundary */
#define	SMALLF		(0.01)		/* Tie distance for two points in map coord*/

#define ONEBLOCK	2048
#define MAXST		80
#define BNDS_FILE	"STATE_BNDS"
#define GREAT_LAKES	"GREAT_LAKES"
#define XML_HDR		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
#define SCM_FILE	"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation="
#define	XSD		"xsd"

#define MT_OBSC_HAZARD  "MT_OBSC"
#define FZLVL_THRES	(7)		/* default threshold for fzlvl reduce points */
#define FZLVL_TOL  	(0.15F)		/* value for fzlvl reduce tolerance */
#define TIE_DISTANCE	(1.0e-4F)	/* distance to identify two close points 
 				      	       the same point in normalized coordinate */
#define FROM_LINE	0
#define BOUNDED_BY_LINE	1

#define	AIRMET_CSTL_BNDS	"AIRMET_CSTL_BNDS"

#define NEAR_FA_BOUND  	(20)		/* (nm) distance close enough to be considered as on the FA bound */ 
#define EXT_DIST	(5000)		/* (nm) distance to extend open FZLVL line */
#define FOUR_THOUSAND	( 40 )		/* or 040 in AWC usage */

#define NO_POINT_REORDER ( -1 )

#define	X_SNAPSHOTS	(0)		/* snapshot intersect FROM/BOUNDED BY line */
#define O_SNAPSHOTS	(1)		/* snapshot not intersect FROM/BOUNDED BY line */

#define FZLVL_MAX_GAP   (100)
#define FZLVL_MIN_LEN   (100)

#define ROUNDUP(x) ( (x) < 0.0 ? (x-0.5) : (x+0.5) )

#define REDUCE_INCR_PCT	(0.0F)
#define REDUCE_INCR_PCT_ORIG    (10.0F)
#define REDUCE_INCR_DST	(0.0F)
#define MAX_REDUCE_PCT  (25)

#define GFA_EPSILON   (1.0e-6F)
#define OUT_EPSILON   (1.0e-2F)

#define CLUSTER_DIST	(30.0F)		/* distance to define clustering points */


#define NUM_SIERRA_HAZARDS	( 2 )	/* number of sierra class GFA hazards */
#define NUM_TANGO_HAZARDS	( 5 )	/* number of tango class GFA hazards */
#define NUM_ZULU_HAZARDS	( 4 ) 	/* number of zulu class GFA hazards */


/*
 *  Private structure
  */
typedef struct		/* GFA SMEAR/OUTLOOK preprocessing structure 
  			    snapshots, smear, and outlook with hazard 
    			    type & tag are grouped */
{
    char		haz_type[8];	/* GFA Hazard type */
    char		tag[4];		/* GFA tag */
    Boolean		openFzlvl;	/* Open of closed FZLVL? */
    int			nsnapshot;	/* Number of snapshots */
    VG_DBStruct		**snapshots;	/* Array of pointers to snapshots */
    VG_DBStruct		*smear;		/* Pointer to smear */  
    VG_DBStruct		*outlook;	/* Pointer to outlook */  
} GFA_SmrOlk_Grp;
						        
typedef struct		/* GFA Element Formatting Structure */
{
    int			delete;		/* Deleted or not 			*/
    char		region;		/* Region ('W', 'C' or 'E') 		*/
    char		area[4];	/* FA Area ("BOS", "MIA"m etc.) 	*/
    char		adjarea[4];	/* Adjacent FA Area("BOS", etc., or "NIL")*/
    VG_DBStruct		el;		/* VG Structure 			*/
    gpc_polygon		*el_poly;	/* GPC polygon Structure 		*/
    Boolean		openLine;	/* Open or closed line			*/
    Boolean		fzlvlContour;	/* True if hazard is fzlvl contour	*/
    int			*reduceFlg;	/* Reduce-able flag for polygon vertices */
    GFA_SmrOlk_Grp	*origInfo;	/* Pointer to original GFA group 	*/
    char		*wording;	/* Accumulated condition information	*/
    int			twin;		/* Index of twins from area rules	*/
} GFA_Elem_Format;

typedef struct		/* GFA airmet wording structure, 'ss' = snapshot */
{
    int		type;		/* snapshot's type - X_SNAPSHOT/O_SNAPSHOT */
    int		hour;		/* hour part of snapshot's forecast hour */
    int		minute;		/* minute part of snapshot's forecast hour */
    int		dvlpg_hr;	/* snapshot's hour for development wording */
    int		endg_hr;	/* snapshot's hour for ending wording */
} GFA_SS_Wording;

typedef struct		/* Structure for conditional wording & af_applyAttr */
{
    int			type;		/* group's type at this forecast time: 	
    				     	 	X-SNAPSHOTS - at least one ss is X	
				      		O_SNAPSHOTS - all SS is O_SNAPSHOTS	
					    */
    int			hour;		/* hour part of the forecast hour */
    int			minute;		/* minute part of the forecast hour */
    int		        dvlpg_hr;	/* group's hour for development wording */
    int		        endg_hr;	/* group's hour for ending wording 	*/
    int			nsnap;		/* number of ss with this forecast hour	*/
    int			start;		/* start index in the ss array 		*/
    GFA_SS_Wording	*ssw;		/* info for each snapshot at this hour	*/
} GFA_SS_Attr;


#ifndef AF_GLOBAL



/*
 * Preference settings in prefs.tbl
 */
extern int	_clippingFlg, _condWording, _reduPtsFlg;
extern float	_reducePct, _reducePctOrig, _reduceDst, _clusterDst;


/*
 * Library declarations
 */

extern gpc_polygon     _intlBndPoly;
extern gpc_polygon     _regionBndPoly[NUM_FA_REGION];
extern gpc_polygon     _areaBndPoly[NUM_FA_AREAS];
extern gpc_polygon     _areaXBndPoly[NUM_FA_AREAS];

extern char	*_FA_Region[ NUM_FA_REGION ];
extern char	*_FA_Area[NUM_FA_AREAS];

extern int	_Sierra_Hazards[ NUM_SIERRA_HAZARDS ];
extern int	_Tango_Hazards[ NUM_TANGO_HAZARDS ];
extern int	_Zulu_Hazards[ NUM_ZULU_HAZARDS ];


/*
 *  FA region bounds in map coordinates as closed lines.
 */
extern Boolean		_boundSet;
extern int		_westP, _centP, _eastP; 
extern float		_westX[MAX_BOUND_PT], _westY[MAX_BOUND_PT], 
                        _centX[MAX_BOUND_PT], _centY[MAX_BOUND_PT],
 		        _eastX[MAX_BOUND_PT], _eastY[MAX_BOUND_PT];

/*
 *  Common borders of w/c regions, and c/e regions (map coord)
 */
extern int		_nwc, _nec; 
extern float		_wcX[MAX_BOUND_PT], _wcY[MAX_BOUND_PT],
 			_ecX[MAX_BOUND_PT], _ecY[MAX_BOUND_PT]; 

/*
 *  Common borders of SLC/SFO extended areas, and CHI_BOS/DFW_MIA.
 */
extern int		_nextdw, _nextdc; 
extern float		_extdwX[MAX_BOUND_PT], _extdwY[MAX_BOUND_PT],
 			_extdcX[MAX_BOUND_PT], _extdcY[MAX_BOUND_PT]; 

/*
 *  Extended FA area bounds in map coordinates as closed lines.
 */
extern int		_sfoP, _slcP, _chibosP, _dfwmiaP; 
extern float		_sfoX[MAX_BOUND_PT], _sfoY[MAX_BOUND_PT], 
                        _slcX[MAX_BOUND_PT], _slcY[MAX_BOUND_PT],
                        _chibosX[MAX_BOUND_PT], _chibosY[MAX_BOUND_PT],
                        _dfwmiaX[MAX_BOUND_PT], _dfwmiaY[MAX_BOUND_PT];


#endif  /* AF_GLOBAL */

/*
 *  library prototypes
 */

void dumpGFA_Elem_Format ( char *str, int *nin, GFA_Elem_Format **fmt_in );

void af_fmt2xml		( int nareas, char areas[][ 8 ], int ntypes, 
			  char *types[3], char *day, char *cycle, int nin, 
                          GFA_Elem_Format *fmt_in, 
                          const float *xCentroid, const float *yCentroid,
                          char *string[NUM_FA_AREAS][NUM_TYPES],
                          int *iret );

void af_fmtStateList    ( const char *hazardType, int npts, 
			  const float *lat, const float *lon,
			  const char *area, const char *adjArea,
			  char *stateList, int *iret );

void af_condsWording	( char *cycle, char *day, int issuance, int numFmt, 
 			  GFA_Elem_Format *fmt, int *iret );

void af_elm2poly	( VG_DBStruct el, gpc_polygon *poly, int *iret );

void af_bnds2poly	( char *bounds, char *name, 
                          gpc_polygon *poly, int *iret );

float af_gpcPolyArea	( gpc_vertex_list *contour, char *sysp );

void af_areaRules	( gpc_polygon *bnds, int *numFmt, 
		          GFA_Elem_Format **fmt, int *iret );

void af_poly2fmt	( gpc_vertex_list contour, VG_DBStruct el,  
                          char region, GFA_Elem_Format *fmt, int *iret );

void af_IFRCombineCIGVIS( int *numFmt, GFA_Elem_Format **fmt, int *iret );

void af_reg2intl	( int *iret );

void af_setBoundInt	( int *iret );

void af_loadCentroids   ( float xCentroid[ NUM_FA_AREAS ],
                               float yCentroid[ NUM_FA_AREAS ], int *iret ) ;

void af_fmtFROMLines    ( int ngrp, GFA_SmrOlk_Grp *gfaGrp, 
			  gpc_polygon *rBnds, gpc_polygon *aBnds, 
			  int *nout, GFA_Elem_Format **fmt_out, int *iret );

void af_fmtOpenFzlvl 	( const gpc_polygon *bnds, const VG_DBStruct *elIn,
		      	  int *numFmt, GFA_Elem_Format **fmt, int *iret );

void af_fmtClosedFzlvl 	( const gpc_polygon *bnds, VG_DBStruct *elIn,
		      	  int *numFmt, GFA_Elem_Format **fmt, int *iret );

void af_elms2grps	( int nin, VG_DBStruct *el_in, int *ngrp, 
			  GFA_SmrOlk_Grp **gfaGrp, int *iret );

void af_reducePts	( int *nin, GFA_Elem_Format **fmt_in, int *iret );

void af_applyAttr 	( char *cycle, int numFmt, 
			  	GFA_Elem_Format *fmt_in, int *iret );

void af_getPolyVerts	( gpc_vertex_list *contour, 
			 int *nout, float *xout, float *yout, int *iret);

void af_queryPrefs	( int *iret );		

void af_replaceIntPt    ( gpc_vertex_list ctr_in, int nip, float *ipx,
                         float *ipy, float *sipx, float *sipy,
                         gpc_vertex_list *ctr_out, int *iret );

void af_getIssueTm 	( char* cycle, char* day, Boolean overrideIssueTm, 
		     		char* issueTm, int *iret );

Boolean af_overrideIssueTm ( const char *area, const char *airmetType, 
			     int nin, const GFA_Elem_Format *fmt_in, 
   			     int *iret );
void af_getSSAttr	( int nin, VG_DBStruct **el_in, gpc_polygon *poly,
			  int *nout, GFA_SS_Attr *ss_attr, int *iret );

void af_initBnds	( int	*iret );

void af_freeBnds	( int	*iret );

void af_parseWording( Boolean isSmear, char *wording, char *beginWording,
			char *endWording, int *iret );

void af_TURBRenameHILO	( char	*hazardType );

#endif  /* AFCMN_H */
