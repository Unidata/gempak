#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "gpc.h"
#include "proto_gpc.h"

#define MAX_BOUND_PT    (500)           /* Max. num. of points for a boundary */
/*
 *  Private structure
 */
typedef struct          /* OUTLOOK preprocessing structure
                           high, mdrt, slgt and see_text risk with cat grouped */ 
{
    int                 nhigh;      	/* number of high areas */
    int                 nmdrt;      	/* number of mdrt areas */
    int                 nslgt;      	/* number of slgt areas */
    int                 ntext;      	/* number of text areas */
    VG_DBStruct         **high;    	/* Array of pointers to high */
    VG_DBStruct         **mdrt;    	/* Array of pointers to mdrt */
    VG_DBStruct         **slgt;    	/* Array of pointers to slgt */
    VG_DBStruct         **text;    	/* Array of pointers to text */
} P2C_Grp;

#define ROUNDUP(x) ( (x) < 0.0 ? (x-0.5) : (x+0.5) )
