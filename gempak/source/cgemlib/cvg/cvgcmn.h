
/************************************************************************
 * cvgcmn.h								*
 * This header file contains the global variables in cvg library.	*
 *									*
 **									*
 * T. Lee/SAIC	 	11/03	Created					*
 * J. Wu/SAIC	 	01/04	add cvggfa.h				*
 * B. Yin/SAIC		02/04	added cvgtca.h                          *
 * J. Wu/SAIC	 	07/04	add display filter variables		*
 * J. Wu/SAIC		10/04   add vgtag.h               		*
 * S. Danz/AWC		03/06   added structures and functions for      *
 *				auto placement and meta-data management *
 * J. Wu/SAIC		06/06   add variables to load filter.tbl     	*
 * S. Danz/AWC		02/07   add new function prototypes     	*
 * E. Safford/SAIC	01/08	add proto_nmaplib.h			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "vgtag.h"
#include "cvgtca.h"
#include "proto_nmaplib.h"


/*
 * Private datastructures to allow the routines that pass info to the 
 * meta-data/placement processing to manage the grouped objects.
 */
typedef struct {
    int     groupid;
    Handle  handle_id;
} cvg_el_group_info;

typedef struct {
    int     refs_used;
    int     objs_used;
    cvg_el_group_info refs[MAX_EDITABLE_ELEMS];
    cvg_el_group_info objs[MAX_EDITABLE_ELEMS];
} cvg_group_info;

typedef struct {
    int     enabled;
    int     obj_type;
    int     obj_subtype;
    int     ref_type; 
    int     ref_subtype; 
    int     inside;
    int     both_sides;
    int     attempts;
    float   offset;
    float   increment;
    int     point_center;
} cvg_placement_settings;

#define	FILE_NAME	".DEFAULT.vgf"

#ifdef USR_PREF_FILE
    char        work_file[LLPATH];
#else
    extern char work_file[LLPATH];
#endif


#ifdef	FILTER_STORAGE
    int			numFilter;
    filter_t		dsplyFilter[MAX_FILTER_NUM];	
    int			nTblFilter;
    filter_t		tblFilter[MAX_FILTER_NUM];	
#else
    extern int		numFilter;
    extern filter_t	dsplyFilter[MAX_FILTER_NUM];	
    extern int		nTblFilter;
    extern filter_t	tblFilter[MAX_FILTER_NUM];	
#endif

#ifdef CVG_META_DATA
    CMDObjectSet    cvg_metadata = NULL;
    PlacementSet    cvg_placements = NULL;
    cvg_group_info  *cvg_group_check = NULL;
    int             cvg_meta_loading = 0;
#else
    extern CMDObjectSet    cvg_metadata;
    extern PlacementSet    cvg_placements;
    extern cvg_group_info  *cvg_group_check;
    extern int             cvg_meta_loading;
#endif

#ifdef CVG_AUTOPLACE_CONF
    int     place_conf_file_time = 0;
    int     cvg_cap_conf_info_valid = 0;
    cvg_placement_settings   cvg_cap_by_type[MAX_RECTYPES];
    cvg_placement_settings   cvg_cap_by_group[MAX_GROUP_TYPE];
#else
    extern int place_conf_file_time;
    extern int cvg_cap_conf_info_valid;
    extern cvg_placement_settings   cvg_cap_by_type[];
    extern cvg_placement_settings   cvg_cap_by_group[];
#endif

/* private funcitons to support CAP and CMD integration */
void cvg_setupplace(int *iret);
void cvg_ld4place(char *filename, int *iret);
void cvg_layer4place(char *filename, int *iret);
void cvg_el2place(int elpos, VG_DBStruct *el, int *iret);
Handle cvg_el2hndl(int elpos, VG_DBStruct *el, int *iret);
void cvg_eladj(int elpos, VG_DBStruct *el, int *iret);
