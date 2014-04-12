/************************************************************************
 * da.h									*
 *									*
 * This file contains the common information for the DA library		*
 * functions - include files, global variables, structures, etc.	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/13	Initial coding				*
 * S. Jacobs/NCEP	12/13	Added structure for saved values (NX,NY)*
 ***********************************************************************/

/* Common include files */
#include "geminc.h"
#include "gemprm.h"

#include <libxml/xmlreader.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

/* Constants */
#define STRSIZE 100	/* String size */

/* Structures and Enums */
/* Enum list for the type of return values from Python */
typedef enum {
    DACHAR,
    DAINT,
    DAFLOAT
} daret_t;

/* Structure for the hash tables */
typedef struct hash {
    char	*key;
    int		val;
} Hash_t;

/* Substructure for the parameter information */
typedef struct parm {
    char	*name;
    int		scale;
    float	offset;
    int		bits;
    char	*key;
} parm_t;

/* Substructure for the part information */
typedef struct part {
    char	*name;
    int		type;
    char	*pyfile;
    char	*pymethdata;
    char	*pymethhdr;
    int		numparms;
    parm_t	parms[MMPARM];
} part_t;

/* Substructure for the part information */
typedef struct fhdr {
    char	*name;
    int		type;
    int		length;
    char	*pyfile;
    char	*pymeth;
    char	*dbkey;
} fhdr_t;

/* Structure for the information to emulate a GEMPAK file */
typedef struct info {
    char	*label;
    int		version;
    int		type;
    int		source;
    char	*dbserver;
    char	*dbtable;
    char	*pyfile_row;
    char	*pymeth_row;
    char	*dbkey_row;
    int		numrows;
    char	*rows[MMKEY];
    char	*pyfile_col;
    char	*pymeth_col;
    char	*dbkey_col;
    int		numcols;
    char	*cols[MMKEY];
    int		numparts;
    part_t	parts[MMPRT];
    int		numfhdrs;
    fhdr_t	fhdrs[MMPRT];
} CommonInfo_t;

/* Structure for any information that needs to be saved for later use */
typedef struct cmn {
    int		nx;
    int		ny;
} dacmn_t;

/* Macros */
/* Find the value for the given key in the hash table */
#define FIND_KEY(thisvalue,thiskey,strname,strtype) \
	{ int ii; \
    	thisvalue = -1; \
	for ( ii = 0; ii < (int)(sizeof(strname)/sizeof(strtype)); ii++ ) { if ( strcmp(strname[ii].key,thiskey) == 0 ) { thisvalue = strname[ii].val; } } }

/* Global variables */
#ifdef DACMN_GLOBAL
#define DAEXT 
/* Hash tables to convert strings to the parameter values */
Hash_t ftype[]   = { {"MFSF",MFSF},
   		     {"MFSN",MFSN},
		     {"MFGD",MFGD} };

Hash_t source[] = { {"MFUNKN",MFUNKN},
   		    {"MFAIRW",MFAIRW}, 
		    {"MFMETR",MFMETR},
		    {"MFSHIP",MFSHIP}, 
		    {"MFBUOY",MFBUOY},
		    {"MFSYNP",MFSYNP}, 
		    {"MFRAOB",MFRAOB},
		    {"MFVAS",MFVAS}, 
		    {"MFCOUN",MFCOUN},
		    {"MFGRID",MFGRID},
		    {"MFTEXT",MFTEXT} };

Hash_t dtype[] = { {"MDREAL",MDREAL},
		   {"MDINTG",MDINTG},
		   {"MDCHAR",MDCHAR},
		   {"MDRPCK",MDRPCK},
		   {"MDGRID",MDGRID} };

#else
#define DAEXT extern
extern Hash_t ftype[];
extern Hash_t source[];
extern Hash_t dtype[];
#endif

/* Type of data to return */
DAEXT	daret_t	datype;

/* Number of input arguments and the argument list */
DAEXT	int	danarg;
DAEXT	char	**daargs;

/* Variables to store the results of the call to Python */
/* The array used is dependent upon the type requested */
DAEXT	char	*daoutc;

DAEXT	int	danumi;
DAEXT	int	*daouti;

DAEXT	int	danumf;
DAEXT	float	*daoutf;

/* The file info structure */
DAEXT	CommonInfo_t common[MMFILE];

/* The saved info structure */
DAEXT	dacmn_t dacmn;
