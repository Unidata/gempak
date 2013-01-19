/*
 * B_Globals - VERSION: %I%  %E% %T%
 */
#ifndef _BUFR_GLOBALS
#define _BUFR_GLOBALS

#include <mel_bufr.h>

/******************************************/
/* BUFR encoder library global variables. */
/******************************************/

/* Flags for debugging and tracing information. */

int BUFR_Debug_Flag = 0;
int BUFR_Trace_Flag = 0;

EndianFlag_t Endian_Flag = BIG_ENDIAN; /* Architecture-sensitive flags */

BUFR_Err_t BUFR_Error;      /* Global BUFR error message structure */

Table0_t  Table0[MAX_TABLE_0_ENTRIES];
TableA_t  TableA[MAX_TABLE_A_ENTRIES];
TableB_t* TableB = (TableB_t*) NULL;
TableD_t* TableD = (TableD_t*) NULL;

BUFR_Cntl_t BUFR_Cntl;

BUFR_Msg_t BUFR_Msg = { NEVER_INITIALIZED };

/* LastVal is used by BUFR_Get_Value() and BUFR_Destroy. */

BUFR_Val_t LastVal =
{
    (double*) NULL,
    (int*) NULL,
    0,
    (FXY_t) BAD_FXY_VAL,
    DT_UNKNOWN
};

/*
 * Memory reference linked list used by BUFR_malloc() and BUFR_free()
 * for memory debugging and analysis if MEMORY_DEBUG is #define'd.
 */

MemoryNode_t Mem_Head = { NULL, NULL, 0, NULL };
MemoryNode_t Mem_Tail = { NULL, NULL, 0, NULL };

#endif
