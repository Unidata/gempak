#ifndef _MEL_BUFR_DEFAULTS_H
#define _MEL_BUFR_DEFAULTS_H

/**********************************************************************/
/***************                                       ****************/
/***************   BUFR DEFAULTS.  CHANGE AS NEEDED.   ****************/
/***************                                       ****************/
/**********************************************************************/
/*
 * 021097 JRA - Added code 128 to Table 0 for Naval Research Lab,
 *              Monterey. Use this for the default originating center
 *              instead of 58 (FNOC).
 * 022398 LAH:  Added enums for flags for use with Set_Flag.c
 *
 *   NOTE: there were probably several modificateion that were not logged 
 *         during this interval.
 *
 * 120402 LAH:  Added Not_All_Numbers_Double and All_Numbers_Double to
 *              the enum for CNTL_OPT_ENUM.
 * 101204 STJ:	Set defaults for NCEP
 */
 
#define DEFAULT_BUFR_EDITION                3
#define DEFAULT_MASTER_TABLE_NUMBER         0
#define DEFAULT_MASTER_TABLE_VERSION_NUMBER 11
#define VAL_ERROR  2147483645
/* Center ID's (must match Table 0 entries). */

enum
{
    NMC_ID      =   7,
    NWSTG_ID    =   8,
    AFGWC_ID    =  57,
    FNOC_ID     =  58, FNMOC_ID = FNOC_ID,
    ECMWF_ID    =  98,
    NRL_MRY_ID  = 128,
    MISSING_ID  = 255
};

/*
 * Default Originating Center: Code is stored in BUFR Master Table 0
 *
 * 57 == AFGWC
 * 58 == FNOC Monterey, CA
 * 98 == ECMWF
 *
 * JRA021097 - Added code 128 to Table 0 for Naval Research Lab, Monterey.
 * Use this for the default originating center instead of 58 (FNOC).
 *
 * Set this value as appropriate.
 */

#define DEFAULT_ORIGINATING_CENTER NMC_ID

/*
 * Default local table version number and minor version number.
 * If local tables are not used, both of these must be set to 0.
 * If minor version numbers for local tables are not used, the minor version
 * must be set to 0.
 */

/*
 * JRA022497: Local and minor local version numbers should be
 * explicitly set in the BUFR_Info_t structure.
#define DEFAULT_LOCAL_TABLE_VERSION_NUMBER 1
#define DEFAULT_MINOR_VERSION_NUMBER       1
*/

#define DEFAULT_LOCAL_TABLE_VERSION_NUMBER 0
#define DEFAULT_MINOR_VERSION_NUMBER       0

#define TABLE_DIR_ENV_VARIABLE "MEL_BUFR_TABLES"
#define BUFR_LOG_DIR_ENV_VARIABLE "MEL_BUFR_LOG"
/*
 * Change these to 9 and 36 on a Honeywell with 9-bit bytes and 36-bit words.
 */

#define BITS_IN_BYTE  8
#define BITS_IN_WORD (sizeof(int)*BITS_IN_BYTE)

/* Maximum length of a file path (directory and file name). */

#define MAX_PATH_LEN 256

/*
 * Increase or decrease bit stream block size as needed but make sure
 * that the block size is a multiple of 4.
 */

#define BIT_STREAM_BLOCK_SIZE 1024

/*
 * 022398 LAH:  Added enums for flags for use with Set_Flag.c
 *              and structure BUFR_Cntl_t
 */
 
#define YES  1
#define NO   0

typedef enum
{
    Allow_Auto_FTP, 
    NoAuto_FTP,
    Allow_Dup_Tab_Entry,
    No_Dup_Tab_Entry,
    Warn_Dup_Tab_Entry,
    No_Warn_Dup_Tab_Entry, 
    Ignore_Dup_Entry, 
    Dont_Ignore_Dup_Entry, 
    BUFR_LOG_ON, 
    BUFR_LOG_OFF, 
    BUFR_LOG_TO_stdout, 
    BUFR_LOG_TO_bufr_log_pid, 
    Print_New_Table_Entries, 
    Do_Not_Print_New_Table_Entries, 
    Dump_Data_Type_11_Msg, 
    Do_Not_Dump_Data_Type_11_Msg, 
    Replace_Code_Table_Missing_Values, 
    Do_Not_Replace_Code_Table_Missing_Values, 
    Replace_Missing_Values, 
    Do_Not_Replace_Missing_Values,
    Not_All_Numbers_Double,
    All_Numbers_Double
}CNTL_OPT_ENUM;

#endif      /* #ifndef _MEL_BUFR_DEFAULTS_H */
