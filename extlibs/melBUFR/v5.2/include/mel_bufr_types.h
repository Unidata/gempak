#ifndef _MEL_BUFR_TYPES_H
#define _MEL_BUFR_TYPES_H

/*******************************/
/* Standard types used by BUFR */
/*******************************/

/*******************************/
/* Global size of file         */
/*******************************/
/* int BUFR_Size_of_File; */

/****************************************************************/
/* TO THOSE DECODING BUFR MESSAGES: The BUFR_Val_t data type is */
/* declared at the end of this file.                            */
/****************************************************************/
/*****************************************************************
 * CHANGE LOG
 *   092997 LAH: Added test for Linux system
 *   12/12/97 LAH:  Added a counter and pointer to last entry in
 *                  FXY_List_t
 *   01/26/98 VLP:  Split originating center into sub-center and
 *		    originating center.
 *   02/23/98 LAH:  Added BUFR_Cntl_t structure to contain controll
 *                  informatiOn
 *
 *   NOTE: there may have been modifications during this period that may 
 *         not have been logged.
 *
 *   05/23/02 LAH:  Add variable int Val_Scale to BUFR_Val_t structure
 *                  to hold descriptor scale values.
 *   12/04/02 LAH:  Changed type of BUFR_Log_File from int to char* in
 *                  bufr_cntl structure.
 * 
 *                  Added cariable int All_Numbers_Not_Double to bufr_cntl
 *                  structure 
 */

#ifndef _SYS_TYPES_H
typedef unsigned char  uchar_t;
typedef unsigned short ushort_t;

#ifndef _HPUX_SOURCE
typedef unsigned long  ulong_t;
#endif

typedef unsigned int   uint_t;
#endif

/* 092997  LAH:  Added following ifdef to check if Linux system.  
*                Apparently Linux does not define these data types
*                in sys/types.h as does other systems.
*/

#ifdef _LINUX_TYPES_H
typedef unsigned char  uchar_t;
typedef unsigned short ushort_t;
typedef unsigned long  ulong_t;
typedef unsigned int   uint_t;
#endif


typedef struct { uchar_t val[3]; } Int3_t;      /* 24-bit integer */

typedef struct { uchar_t val[2]; } Int2_t; /* 16 bit integer, endian independent */

typedef uchar_t* HexStr_t;                      /* String of hex digits */

typedef struct af_entry
{
    int nbits;  /* Bit width of associated field */
    int sig;    /* Corresponds to 0-31-21 */

    struct af_entry *next;

} AF_Entry_t;

typedef struct
{
    AF_Entry_t* head;
    AF_Entry_t* tail;

} AF_List_t;

typedef struct
{
    unsigned char* buffer;
    int            size;        /* Number of bytes allocated to buffer */
    unsigned char* bp;          /* Current pointer                     */
    int            byte_num;    /* Current byte (bp-buffer)            */
    int            bit_num;     /* Current bit, ranges 0 to 7          */

} BitStream_t;

#define MAX_BUFR_ERR_LOG 256

typedef struct
{
    char* Log[MAX_BUFR_ERR_LOG];    /* Functions logging error         */
    int   NumLogs;                  /* Length of FunctionLog array     */
    char* Message;                  /* Description of error condition  */
    int   Set;                      /* Flag indicating BUFR error      */
    int   SystemError;              /* System error number when called */

} BUFR_Err_t;

typedef enum
{
    DT_UNKNOWN = 0,

    /*
     * Eventually, I may decide to use the lower 3 bytes to store a number
     * indicating repetitions of data types (i.e., DT_INT|250 instead of 250
     * individual DT_INT's).  Additionally, I may support pointers and
     * pointers to pointers (e.g. DT_INT_PTR, DT_CHAR_PTR_PTR).
     */

    DT_CHAR   = 1 << 24,
    DT_SHORT  = 2 << 24,
    DT_LONG   = 3 << 24,
    DT_FLOAT  = 4 << 24,
    DT_DOUBLE = 5 << 24,

    DT_STRING = 6 << 24,

    DT_UCHAR  = (0x10 << 24) | DT_CHAR,
    DT_USHORT = (0x10 << 24) | DT_SHORT,
    DT_ULONG  = (0x10 << 24) | DT_LONG,

    /* Alternate names. */

    DT_UNSIGNED_CHAR  = DT_UCHAR,
    DT_UNSIGNED_LONG  = DT_ULONG,
    DT_UNSIGNED_SHORT = DT_USHORT,

    DT_INT   = DT_LONG,
    DT_INT16 = DT_SHORT,
    DT_INT32 = DT_LONG,

    DT_UINT         = DT_ULONG,
    DT_UNSIGNED     = DT_UINT,
    DT_UNSIGNED_INT = DT_UINT

} DataType_t;

/* Encoded BUFR value */

typedef struct
{
    HexStr_t value;     /* Hex string representing encoded value. */
    int      nbits;     /* Size of encoded value, in bits.        */
} EncVal_t;

#ifndef LITTLE_ENDIAN && BIG_ENDIAN
typedef enum
{
    UNKNOWN_ENDIAN,
    LITTLE_ENDIAN,      /* Intel 80x86, PowerPC, etc. */
    BIG_ENDIAN,         /* SPARC, Motorola, IBM 360/370, etc. */
    MIDDLE_ENDIAN       /* DEC PDP-11, VAX, etc. */
} EndianFlag_t;
#endif /* LITTLE_ENDIAN or BIG_ENDIAN */

typedef enum
{
    YYYYMMDD,
    MMDDYYYY,
    DDMMYYYY
} Date_Format_t;


typedef unsigned long FXY_t;        /* Packed FXY values */

typedef struct fxy_entry
{
    FXY_t             fxy;
    struct fxy_entry* next;
} FXY_Entry_t;

/* 12/11/97 LAH: Added two new elements to structure   
 *      last = structure pointer to track last entry in linked list 
 *   num_fxys = counter for number of FXYs in linked list.  
 *
 *       These elements were added so that teh linked list did not have 
 *       to be traversed every time this information was needed
 */
typedef struct
{
    FXY_Entry_t* head;
    FXY_Entry_t* tail;
    FXY_Entry_t* last;
    int  num_fxys;
} FXY_List_t;

typedef enum
{
    NEVER_INITIALIZED,
    NOT_INITIALIZED,
    BEEN_INITIALIZED
} InitStatus_t;

/*
 * Processing flag used to prevent mixing incompatible processing methods.
 * Incompatible methods include trying to decode data while encoding,
 * mixing raw BUFR data with single values, etc.
 */

typedef enum
{
    TYPE_UNKNOWN,
    TYPE_ENCODE,
    TYPE_DECODE,

    /* User-specified values for BUFR_Init(). */

    ENCODE = TYPE_ENCODE,
    DECODE = TYPE_DECODE,

    ENCODING = ENCODE,
    DECODING = DECODE

} ProcFlag_t;

typedef enum
{
    /*
     * Internal flags specifying the method of processing.
     *
     * METHOD_UNKNOWN - Until a BUFR_Get_*() or BUFR_Put_*() function is
     * called, this should be the method.
     *
     * METHOD_VALUES - Process FXY value/data pairs one-at-a-time.  Used by
     * BUFR_Put_Array(), BUFR_Put_Value(), and BUFR_Get_Value().
     *
     * METHOD_RAW - Process "raw" data (data already BUFR-encoded).
     * Encoded by BUFR_Put_OptData(), BUFR_Put_S3Data() and BUFR_Put_S4Data().
     * Decoded by BUFR_Get_OptData(), BUFR_Get_S3Data() and BUFR_Get_S4Data().
     *
     * METHOD_TEMPLATE - Template-based encoding.  See BUFR_Define_Dataset()
     * for an in-depth explanation.
     *
     */

    METHOD_UNKNOWN,
    METHOD_VALUES,
    METHOD_RAW,
    METHOD_TEMPLATE

} MethFlag_t;

typedef enum
{
    UNKNOWN_UNIT,
    CCITT_IA5,
    CODE_TABLE,
    FLAG_TABLE,
    NUMERIC
} Units_t;

/* Status codes used for decoding. */

typedef enum
{
                        /* Decoding meaning: */
    BUFR_EOD    = -3,   /* end of data set - still data in message    */
    BUFR_EOM    = -2,   /* end of message  - more messages in file    */
    BUFR_EOF    = -1,   /* end of file     - no more messages in file */
    BUFR_OK     =  0,
    BUFR_ERROR  =  1

} Status_t;

typedef struct val_entry
{
    int               val;
    struct val_entry *next;

} ValEntry_t;

typedef struct
{
    ValEntry_t* head;
    ValEntry_t* tail;

} ValStack_t;

/**********************/
/* Section structures */
/**********************/

/*
 * Section 0 - Indicator Section.
 */

typedef struct
{
    char    id[4];              /* Set to 'BUFR' */
    Int3_t  message_length;     /* Total length of BUFR message */
    uchar_t edition;            /* BUFR edition number (currently 2) */

} Section0_t;

/*
 * Section 1 - Identification Section.
 */

typedef struct
{
    Int3_t   length;                    /* Section length, in bytes */
    uchar_t  master_table;              /* 0 if standard WMO codes used */
    uchar_t  sub_center;        	/* Code table 0-01-031 */
    uchar_t  originating_center;        /* Code table 0-01-031 */
    uchar_t  update_sequence;           /* 0 for original message */

    /*
     * Section 1 flags:
     *
     * Bit(s)     Meaning
     * ------     ----------------------------------------
     *     1 == 0 No Optional Section (Section 2)
     *     1 == 1 Optional Section follows

     *   2-8      RESERVED (set to 0)
     */

    uchar_t  flags;

    uchar_t  data_category;             /* Table A */
    uchar_t  data_sub_category;         /* Defined by local ADP centers */
    uchar_t  master_table_version;      /* Currently 3 */
    uchar_t  local_table_version;

    uchar_t  year;                      /* Year of century */
    uchar_t  month;
    uchar_t  day;
    uchar_t  hour;
    uchar_t  minute;

} Section1_t;

/*
 * Section 2 - Optional Section.
 */

typedef struct
{
    Int3_t  length;     /* Section length, in bytes */
    uchar_t reserved;   /* Set to 0 */

} Section2_t;

/*
 * Section 3 - Data Description Section.
 */

typedef struct
{
    Int3_t   length;        /* Section length, in bytes */
    uchar_t  reserved;      /* Set to 0 */
    Int2_t data_subsets;  /* Number of data subsets */

    /*
     * Section 3 flags:
     *
     * Bit(s)     Meaning
     * ------     ----------------------------------------
     *     1 == 0 Other Data
     *     1 == 1 Observed Data
      *
     *     2 == 0 Non-compressed data
     *     2 == 1 Compressed data
     *
     *   2-8      RESERVED (set to 0)
     */

    uchar_t flags;

} Section3_t;

/*
 * Section 4 - Data Section.
 */

typedef struct
{
    Int3_t  length;     /* Section length, in bytes */
    uchar_t reserved;   /* Set to 0 */

} Section4_t;

/*
 * Section 5 - End Section.
 */

typedef struct
{
    char id[4];         /* Set to '7777' */

} Section5_t;


/***************************/
/* BUFR message structures */
/***************************/

typedef struct
{
    /* Master Table information. */

    int     BUFR_Edition;                   /* Section 0, Byte  8 */
    int     BUFR_MasterTable;               /* Section 1, Byte  4 */
    int     VersionNumberOfMasterTables;    /* Section 1, Byte 11 */

    /* Local Table information. */

/*  The new WMO standard now has a subcenter and an originating center */
    int	    SubCenter;			    /* Section 1, Byte 5 */
    int     OriginatingCenter;              /* Section 1, Byte 6 */
    int     VersionNumberOfLocalTables;     /* Section 1, Byte 12 */
    uchar_t MinorLocalVersion;              /* Section 1, Byte 18 (if != 0) */

    /*
     * JRA020497 - Added GeneratingCenter.  This is the center that originally
     * generated the data and may be different from the OriginatingCenter
     * (which created the BUFR message but not necessarily the data).  I
     * realize that Originating/Generating Center naming seems contradictory
     * but the naming is historical and there's little I can do about it.
     *
     * If the minor local table version number is non-zero it will appear in
     * octet 18 of Section 1 and the Generating Center ID will appear in
     * octets 19-20.  If the minor local table version number is zero (meaning
     * that there is no version number), then no Generating Center ID is
     * present.
     *
     * When encoding, if the Generating Center is not specifically set in
     * the BUFR_Info_t structure, it defaults to the Originating Center.
     */

    int     GeneratingCenter;               /* Section 1, Bytes 19-20 */

    int     UpdateSequenceNumber;           /* Section 1, Byte  7 */
    int     DataCategory;                   /* Section 1, Byte  9 */
    int     DataSubCategory;                /* Section 1, Byte 10 */

    int     Year;                           /* Section 1, Byte 13 */
    int     Month;                          /* Section 1, Byte 14 */
    int     Day;                            /* Section 1, Byte 15 */
    int     Hour;                           /* Section 1, Byte 16 */
    int     Minute;                         /* Section 1, Byte 17 */
    int	    Century;			    /* Section 1, Byte 18 */
    int	    SoftVNum;			    /* Section 1, Byte 19 (Maybe)*/
    int	    SoftV2Num;			    /* Section 1, Byte 20 (Maybe)*/

    int     ObservedData;                   /* Section 3, Byte  7 */
    int     Num_Data_Sets;                  /* Section 3, Bytes 5&6 */
    /*
     * JRA022697: If the AutoFTP flag is set, then fetch any missing local
     * tables via anonymous FTP.
     */

    int AutoFTP;

    /*  VLP052897:  Set a user specified value as missing data, otherwise
        use the default -999999. as a missing data flag */
 
    double MissingValue;

} BUFR_Info_t;

typedef struct data_entry
{
    /* Values to be written to Section 3 */

    FXY_t* fxy_list;            /* For user-defined structures */
    int    num_fxys;            /* Length of fxy_list          */

    /* Values to be written to Section 4 */

    EncVal_t* value;        /* malloc()'d array of encoded values */
    int       num_values;   /* Used for arrays                    */

    struct data_entry* next;

} DataEntry_t;

typedef struct
{
    DataEntry_t* head;
    DataEntry_t* tail;

} DataList_t;

/********************************/
/* Data types used for decoding */
/********************************/

typedef union
{
    double number;      /* If Value_Type == DT_DOUBLE */
    float  ffloat;	/* If Value_Type == DT_FLOAT */
    char*  string;      /* If Value_Type == DT_STRING */
    int	   int_number;  /* If Value_type == DT_INT */
    short  short_num;	/* If Value_type == DT_SHORT */
} Data_Val_t;

typedef struct bufr_val
{
    double*     AF;         /* Array of associated fields */
    int*        AF_sig;     /* Associated field significance */
    int         num_AFs;    /* Length of AF and AF_sig arrays */

    FXY_t       FXY_Val;    /* Descriptor for Data_Value */

    DataType_t  Val_Type;   /* Valid data types  */

    /* LAH 052302 added scale to structure */
    int Val_Scale; /* scale of values */

    Data_Val_t  Val;        /* Use Value.number if Value_type == numeric */

    short	missing_flag; /* missing value indicator */

    struct      bufr_val *next;
} BUFR_Val_t;

/****************/
/* BUFR Message */
/****************/

typedef struct
{
    InitStatus_t InitStatus;            /* Used to recycle this structure. */

    char       FileName[MAX_PATH_LEN];  /* Name of BUFR message file */
    FILE*      FilePtr;                 /* NULL if file is not open  */

    ProcFlag_t ProcFlag;                /* Processing type   for FileName */
    MethFlag_t MethFlag;                /* Processing method for FileName */

    double Missing_Value;
    int Missing_User_Set;  /*  Flag for whether the user has set a missing */
    int LocalTable_Flag;       /* Set to 1 if internal local table */
    int Multiple_Msg_Flag;  /* User set flag if multiple message are written */
    int Compress_Flag;	    /* Set to 1 if compression is requested */

    /*
     * These file and message status flags are used only for decoding.
     *
     * The meanings of FileStatus values are:
     *
     *     BUFR_OK  - No messages have been read.
     *     BUFR_EOM - There are more BUFR messages in FileName.
     *     BUFR_EOF - There are no more BUFR messages in FileName.
     *
     * The meanings of MsgStatus values are:
     *
     *     BUFR_OK  - There are more values in the current dataset.
     *     BUFR_EOD - The last value in the dataset has been reached
     *                but more datasets remain.
     *     BUFR_EOM - There are no more values in the current BUFR
     *                message but more messages remain.
     *     BUFR_EOM - There are no more values in the current BUFR
     *                message and no more messages remain.
     *
     */

    Status_t   FileStatus;              /* Set by BUFR_Read_Msg() */
    Status_t   MsgStatus;               /* Set by FXY_PtrInc()    */

    BUFR_Info_t Info;

    /* BUFR master table filenames. */

    char* MasterTable0;
    char* MasterTableA;
    char* MasterTableB;
    char* MasterTableD;

    /* BUFR local table filenames. */

    char* LocalTableB;
    char* LocalTableD;

    ValStack_t DataWidthStack;          /* For 2-01-YYY */
    ValStack_t ScaleStack;              /* For 2-02-YYY */

    AF_List_t* af_list;                 /* Associated fields list */

    Section0_t  Section0;

    Section1_t  Section1;
    BitStream_t Section1_Data;

    Section2_t  Section2;
    BitStream_t Section2_Data;

    Section3_t  Section3;
    BitStream_t Section3_Data;

    Section4_t  Section4;
    BitStream_t Section4_Data;

    Section5_t  Section5;

    /* Members used for encoding. */

    DataList_t*  data_list;             /* Linked-list of data values. */

    /*
     * JRA021397 - Added last_de to point to the last entry in data_list.
     * Profiling revealed that during encoding, an enormous amount of
     * time was consumed by calling DataList_Last() in DataList_Put().
     */

    DataEntry_t* last_de;               /* Last entry in data_list.    */

    /* Members used for decoding and template-based encoding. */

    int          subset_index;          /* Used for multiple data subsets.  */
    FXY_t*       subset_fxys;           /* Array of unexpanded FXY values.  */
    int          subset_size;           /* # FXYs in data_list for subset.  */
    FXY_List_t*  exp_fxy_list;          /* List of expanded FXY values.     */
    FXY_Entry_t* exp_ptr;               /* Current pointer to exp_fxy_list. */
    int          rebuild_exp_fxy_list;  /* 1 if any delayed replicators.    */
    
    /*
     * 022498 LAH:  Added to allow logging of messages to a specified 
     *              directory
     */
    FILE         *bufr_log;
    BUFR_Val_t   *decom_vals;  /*array of structures with decompressed Vals */
    int          dc_numb;      /*number of table B FXYs in each decomressed data set */
    int          dc_parm_cnt;  /*Table B counter used in retrieving compressed */
                               /* values to determine end of data set */
} BUFR_Msg_t;


typedef struct
{
    short count;
    BUFR_Val_t *head;
    BUFR_Val_t *tail;
    BUFR_Val_t *last;
} BUFR_DataSet_t;

typedef struct
{
    DataType_t  Val_Type;   /* Either DT_DOUBLE or DT_STRING */

    Data_Val_t  Val;        /* Use Value.number if Value_type == DT_DOUBLE */
} Data_MixVal_t;

/* Data types used for memory debugging if MEMORY_DEBUG is #defined. */

typedef struct mnode
{
    char*  beg_addr;    /* Beginning address of allocated memory. */
    char*  end_addr;    /* Ending    address of allocated memory */
    int    ref_count;   /* Used to see if memory is properly deallocated. */
    struct mnode* next;

} MemoryNode_t;

typedef struct fxy_d
{
     FXY_t   fxy_value;
    Units_t units_type;
    char*   description;
}  FXY_Descriptor_t;

/*
 * LAH022098  Added control parameter structure
 */
 typedef struct bufr_cntl
 {
     int Auto_FTP;
     int Dup_Tab_Entry;
     int Dup_Tab_Entry_Warn;
     int Dup_Tab_Entry_Ignore;
     int BUFR_Log_Flag;
     char *BUFR_Log_File;
     FILE *bufr_log;
     int Print_New_Entries;
     int Dump_Data_Type_11_Msg;
     int Replace_Code_Table_Missing;
     int Replace_Missing_Values;
     int All_Numbers_Not_Double;
     int num_messages;
     double User_Missing_Value;
     int  Missing_User_Set;
 } BUFR_Cntl_t;
 
#endif      /* #ifndef _MEL_BUFR_TYPES_H */
