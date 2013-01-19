/************************************************************************
 * clocmn.h								*
 *                                                                      *
 * This file contains definitions used by the GEMLIB clo functions.	*
 *                                                                      *
 **                                                                     *
 * D.W.Plummer/NCEP	 6/97	Created					*
 * D.W.Plummer/NCEP	10/97	Add county hotlist feature		*
 * D.W.Plummer/NCEP	11/97	Updated for VOR and ANCHOR points	*
 * F. J. Yen/NCEP	09/98	Updated for SFSTN points		*
 * D.W.Plummer/NCEP	12/98	Updated for sorted CITY points		*
 * D.W.Plummer/NCEP	12/98	Furthur update for sorted CITY points	*
 * D.W.Plummer/NCEP	 1/99	Consolidate ANCHOR,VOR,SFSTN types	*
 * D.W.Plummer/NCEP	 2/99	Added COUNTY_BNDS type			*
 * D.W.Plummer/NCEP	 3/99	Add sort index to CountyInfo_t,StnInfo_t*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE and COASTAL types	*
 * S. Law/GSC		08/99	Included vgstruct.h			*
 * T. Piper/GSC		09/99	Added theta macro			*
 * D.W.Plummer/NCEP	11/99	Changed table name for SFSTNS		*
 * S. Jacobs/NCEP	12/99	Removed THETA macro			*
 * D.W.Plummer/NCEP	 8/00	Major revision - added boundary structs,*
 * 				changed station structs,		*
 *				removed city and county references	*
 * D.W.Plummer/NCEP	 4/01	Changes for bound info access		*
 * D.W.Plummer/NCEP	 1/02	Added tBndName and tBndData global vrbls*
 * J. Wu/SAIC	 	 6/05	add _mxspts & _mxbpts			*
 ***********************************************************************/

#ifndef _clocmn_include
#define _clocmn_include

/*
 *  STATION TABLE STRUCTURE
 */

typedef struct sinfo_t
{
    char	*id;		/* Identifier  				*/
    int		nm;		/* Block/station number			*/
    char	*desc;		/* Description				*/
    char	*state;		/* State            			*/
    char	*cntry;		/* Country            			*/
    float	lat;		/* Latitude 				*/
    float 	lon;		/* Longitude				*/
    int 	elv;		/* Elevation				*/
    int 	pri;		/* Priority				*/
    char 	*col10;		/* Column 10				*/
    
} SInfo_t;

typedef struct stn_t
{
    int		nstn;		/* Number of stations 			*/
    SInfo_t	*station;	/* Station information structure	*/
    
} Stn_t;

/*
 *  BOUNDS INFORMATION STRUCTURES
 *
 *  Information is not read in (too much information); 
 *  rather, informational pointers into the file are saved.
 *
 *  Boundaries are pre-sorted east-to-west by easternmost longitude.
 */

typedef struct bndsprt_t  {
    long        strec;          /* Starting record of boundary part	*/
    float       minlat;         /* Minimum latitude of boundary part 	*/
    float       minlon;         /* Minimum longitude of boundary part 	*/
    float       maxlat;         /* Maximum latitude of boundary part 	*/
    float       maxlon;         /* Maximum longitude of boundary part 	*/
    int         npts;           /* Number of points                     */

} Bndsprt_t;

typedef struct binfo_t  {
    char        *name;          /* Boundary name                   	*/
    char        *info;          /* Boundary info                   	*/
    long	strec;		/* Starting record for boundary		*/
    float       cenlat;         /* Boundary centroid (latitude)         */
    float       cenlon;         /* Boundary centroid (longitude)        */
    float       minlat;         /* Minimum latitude of boundary 	*/
    float       minlon;         /* Minimum longitude of boundary 	*/
    float       maxlat;         /* Maximum latitude of boundary 	*/
    float       maxlon;         /* Maximum longitude of boundary	*/
    int		nparts;		/* Number of boundary parts		*/
    Bndsprt_t   *bndspt;        /* Boundary information                 */

} BInfo_t;

typedef struct bnd_t  {
    char	 *filename;     /* Boundaries filename			*/
    int		 nbnd;	 	/* Number of boundaries overall 	*/
    int		 maxpts;	/* Maximum number of points per bndry	*/
    BInfo_t      *bound;	/* Boundary information structure	*/
    
} Bnd_t;


/*
 *  ***  CLO TOP LEVEL STRUCTURES  ***
 */

typedef	struct	cloinfo_t
{
    char	*name;		/* Parameter name			*/
    int		format;		/* Parameter format type		*/
    char	*file;		/* Parameter table/file name		*/
    Stn_t	stn;		/* Info structure for standard stn tbls	*/
    Bnd_t	bnd;		/* Info structure for bounds file	*/
} CLOinfo_t;

typedef	struct	clo_t
{
    int		nloc;		/* Number of CLO structures in use	*/
    CLOinfo_t	*loc;		/* CLO structures			*/
} CLO_t;


/*
 *  CLO SORTING DEFINITIONS
 */
#define	STN_ID		1	/* Station ID			*/
#define	STN_NM		2	/* Station Name			*/
#define	STN_DESC	3	/* Station Descriptor		*/
#define	STN_ST		4	/* Station State		*/
#define	STN_LAT		6	/* Station Latitude		*/
#define	STN_LON		7	/* Station Longitude		*/
#define	STN_ELV		8	/* Station Elevation		*/
#define	STN_PRI		9	/* Station Priority		*/
#define	STN_COL10	10	/* Station Column 10		*/
#define	BND_NAME	1	/* Boundary Name		*/
#define	BND_STREC	2	/* Boundary Starting Record	*/
#define	BND_CLAT	3	/* Boundary Centroid Latitude	*/
#define	BND_CLON	4	/* Boundary Centroid Longitude	*/
#define	BND_MNLAT	5	/* Boundary Minimum Latitude	*/
#define	BND_MXLON	8	/* Boundary Maximum Longitude	*/

/*
 *  UNIVERSAL HOTLIST
 */

#define	MAXHOT	3500
extern	int	nhot, hotlist[MAXHOT];
extern	long	hotstrec[MAXHOT];

extern	int	_mxspts;	/*Max. number of stations*/ 
extern	int	_mxbpts;	/*Max. number of bound points*/

/*
 *  STATION STORAGE
 */

extern	int	whichStn;
extern	int	sortStn;
extern	int	npStn;
extern	float	*latStn, *lonStn;

/*
 *  BOUNDS STORAGE
 */

extern	int	whichBnd;
extern	int	sortBnd;
extern	int	npBnd;
extern	float	*xpBnd, *ypBnd;
extern	float	latllBnd, lonllBnd, laturBnd, lonurBnd;
extern	int	boundBnd, bndsptBnd;
extern	char	*tBndName, *tBndData;
extern  FILE	*fpBnd;

#include "proto_clo.h"
#endif
