/************************************************************************
 * ghcmn.h                                                              *
 * Contains the color table information used by GPTPC.			*
 *                                                                      *
 **                                                                     *
 *Log:                                                                  *
 * A. Hardy/GSC          6/01   Created					*
 * m.gamazaychikov/SAIC	10/03	Added prototypes for GH funtions	*
 * m.gamazaychikov/SAIC	04/04	Added prototypes for gh_bkrv		*
 * m.gamazaychikov/SAIC	08/04	Added numbkp to gh_bkrv prototype	*
 * B. Yin/SAIC          12/04   Changed call seq of gh_bkrv             *
 * D. Kidwell/NCEP       2/05   Added area definitions                  *
 * B. Yin/SAIC          04/05   Changed calling seq of gh_bkrv          *
 * D. Kidwell/NCEP       4/05   Added IKEYS definition                  *
 * S. Gilbert/NCEP       4/05   modified gh_bkrv prototype.             *
 * m.gamazaychikov/SAIC	06/07	Add gh_wtcb, gh_wtce, gh_wtct prototypes*
 * m.gamazaychikov/SAIC 10/08	Add stsrc to gh_wtct prototype          *
 * A. Krautkramer/NHC	 5/09	Add time zone to gh_wtct prototype	*
 ***********************************************************************/

#ifdef TPCGLOBAL

        char			*table;
                                /* Color table entries */

#else

        extern char		*table;

#endif

/* Area definitions */

#define IUSGEC 		1
#define IMXCSA 		2
#define ICUBA  		3
#define IHISP  		4
#define IOTHER 		5
#define IPACIF 		6
#define IPRICO		7
#define IWATER		8
#define IKEYS		9

/* Prototypes for GH library private functions */

void gh_bkrv ( 	char 		*filnam,  
		int 		*istnum,  
		int 		*iyear,    
		int 		*ibasin,
              	char  		*iadvnm,  
		int 		*isttyp,  
		char 		*stname,  
		char 		*vtime,
              	int  		*icount, 
		int 		isev[], 
		int 		ityp[], 
		int 		igeog[],
              	float 		lat[], 
		float 		lon[], 
		char 		*bpnam,
		int 		numbkp[],
		char		*tzone,
		char		*status,
              	int   		*iret );

void gh_gclr (	char 		*tag, 
		int 		*idef, 
		int 		*icolr, 
		int 		*ired,
		int 		*igreen, 
		int 		*iblue, 
		int 		*iret );

void gh_wtcb (  char 		*filnam,  
	        char 		*sev, 
		char 		*vtime, 
		char 		*stnum,
                char 		*stname,  
		char 		*wbas,  
		char 		*advnm,
                char 		*fpd,
                int 		*ilcl,  
		int 		*iltp, 
		int 		*itcww,
                int 		*icount, 
		float 		lat[],   
		float 		lon[],
                int 		*iret);

void gh_wtce ( 	char 		*filnam,  
	  	char 		*sev, 
		char 		*vtime, 
		char 		*stnum,
               	char 		*stname,  
		char 		*wbas,  
		char 		*advnm, 
		char 		*fpd,
               	int 		*ilcl,  
		int 		*iltp, 
		int 		*ifcl,
	 	int 		*iftp,
               	int 		*icount, 
		float 		lat[],   
		float 		lon[], 
		int 		*iret);

void gh_wtct (  char            *filnam,
		char		*zone,
                char            *sev,
                char            *vtime,
                char            *stnum,
                char            *stname,
                char            *wbas,
                char            *advnm,
                char            *fpd,
                int             *ilcl,
                int             *iltp,
                int             *icount,
                float           lat[],
                float           lon[],
                char            *advtm,
                char            *tau,
                char            *mxwnd,
                char            *wgust,
                char            *presr,
                char            *tcdv,
                char            *dvlbl,
                char            *tcdir,
                char            *tcspd,
                char            *dtlbl,
                char            *stsrc,
                int             *iret);

void gh_wwat ( 	char 		*strin, 
		char 		*strout, 
		int 		*ilenout, 
		int 		*iret);

void gh_wwug ( 	char 		*strin, 
		char 		*dd, 
		char 		*hr, 
		char 		*strout,
		int 		*ilenout, 
		int 		*iret);

/* End Prototypes */
