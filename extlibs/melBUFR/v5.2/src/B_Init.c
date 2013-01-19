/*
 * BUFR_Init - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Init - Initialize BUFR message.  Return 1 on error, else 0.
 *
 * If file names are NULL, the proper file to read will be determined.
 * If file names are not NULL, the given file will be read.
 *
 * BM->Section0 to BM->Section5 are not initialized; the encoder or decoder
 * will do this.
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 *
 * 100897  LAH: Added uchar_t cast for BI->BUFR_Edition ~ line 247
 *              Added int cast for ftell ~ line 290
 * 101097  LAH: Made hex constants 32 bits
 * 102097  LAH: Added int cast for enum
 * 022398 LAH: Check if AutoFTP flag in BI has been set by user.
 * 022498 LAH:  Added prints to bufr_log file.
 * 050798 LAH:  Moved test if previously initialized to include
 *              flag testing and log file creation.
 *              Extracted core initilization code and put in new function
 *              BUFR_Init_Sub.  This was required to allow for a second
 *              initialization if new table entries were read from the
 *              first message in the file (data type = 11) and the 
 *              message is not dumped.
 * 100898 VLP:  Added a compress flag initialization
 */

int BUFR_Size_of_File;

#include <time.h>
#include <mel_bufr.h>
extern EndianFlag_t Endian_Flag;
extern BUFR_Msg_t   BUFR_Msg;
extern MemoryNode_t Mem_Head, Mem_Tail;
extern BUFR_Cntl_t BUFR_Cntl;

#if _WIN32
#include <process.h>
#endif

#if PROTOTYPE_NEEDED

int BUFR_Init( BUFR_Info_t* BI, char* BUFR_File, ProcFlag_t ProcFlag )

#else

int BUFR_Init( BI, BUFR_File, ProcFlag )
BUFR_Info_t* BI;            /* Contains requred BUFR information. */
char*        BUFR_File;     /* BUFR file to be processed. */
ProcFlag_t   ProcFlag;      /* Must be ENCODE or DECODE */

#endif
{
    char default_log_dir[MAX_PATH_LEN];
    char *log_dir;
    char log_file[MAX_PATH_LEN];
    time_t now;
    ulong_t  ul;
    uchar_t* uc;

    BUFR_Msg_t* BM;

    char buf[128];
    unsigned char*  s4bp_save;
    BM = &BUFR_Msg;
 
    if( BM->InitStatus == NEVER_INITIALIZED )
    {
     
      /* 
      * 022398 LAH: Check if AutoFTP flag in BI has been set by user.
      *             If so reset corresponding flag in BUFR_Cntl
      */
      if ( BI->AutoFTP >= 0)
        BUFR_Cntl.Auto_FTP = BI->AutoFTP;
    
      /*
      * 022498 LAH: Check is directory has been specified for bufr_log file
      *             If not use local directory.  Open bufr_log and place 
      *             file pointer in BUFR_Msg structure.
      */
      memset( default_log_dir, 0, MAX_PATH_LEN );
    
      /* bufr_log to go to stdout or file */
      if ( BUFR_Cntl.BUFR_Log_Flag )
      {
        BUFR_Cntl.bufr_log = stdout;
        strcpy(log_file,"stdout");
      } else {
        if ( (log_dir=getenv(BUFR_LOG_DIR_ENV_VARIABLE)) != NULL )
        {
          strcpy( default_log_dir, log_dir );
          sprintf(log_file, "%s/bufr_log.%d", default_log_dir, (int) getpid() );
        } else{
	        sprintf(log_file, "bufr_log.%d", (int) getpid() );
        }
        if( (BUFR_Cntl.bufr_log = fopen(log_file, "w")) == NULL )
        {
          sprintf( buf,"  %s: Can't open log file.", log_file);
          BUFR_Err_Set( "BUFR_Init", buf );
          BUFR_Destroy(1);
          return 1;
        }
      }
      BUFR_Cntl.BUFR_Log_File = BUFR_strdup(log_file); 

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
    {
       fprintf(BUFR_Cntl.bufr_log, ">>> Enteriing BUFR_Init\n");
    }
#endif

      time(&now);
      fprintf(BUFR_Cntl.bufr_log, "BUFR LOG: %s\n", ctime(&now) );
      fprintf(BUFR_Cntl.bufr_log, "Processing file %s\n", BUFR_File);
      fprintf(BUFR_Cntl.bufr_log, "log file = %s\n", log_file);
      Flag_Print(BUFR_Cntl.bufr_log);
    
      /* Make sure pointers are all NULL (i.e., no bad addresses). */
      (void) memset( (char*) BM, 0, sizeof(BUFR_Msg) );

      BM->InitStatus = NOT_INITIALIZED;

      /*
      * Determine the byte ordering for this machine.  A high-endian
      * machine will store the value 0x12345678 in an unsigned long as
      * 0x12345678 and a low-endian machine will store this value as
      * 0x34127856.  Any other storage is middle-endian.
      *
      * NOTE: Endian architecture doesn't appear to be an issue for
      * this library but keep the Endian_Flag around just in case to
      * allow the isolation of endian-specific code.
      */

      ul = 0x12345678;
      uc = (uchar_t*) &ul;
      if( uc[0]==0x00000012 && uc[1]==0x00000034 &&
          uc[2]==0x00000056 && uc[3]==0x00000078 )
      {
        Endian_Flag = BIG_ENDIAN;
      } else if( uc[0]==0x00000034 && uc[1]==0x00000012 &&
        uc[2]==0x00000078 && uc[3]==0x00000056 )
      {
        Endian_Flag = LITTLE_ENDIAN;
      } else {
        Endian_Flag = MIDDLE_ENDIAN;
      }

#if DEBUG_PRINT
      if( BUFR_DebugLevel() > 1 )
      {
        fprintf(BUFR_Cntl.bufr_log,"Byte ordering on this machine is " );
        switch( Endian_Flag )
        {
          case LITTLE_ENDIAN:
            fprintf(BUFR_Cntl.bufr_log, "little" );  break;
          case BIG_ENDIAN:    
            fprintf(BUFR_Cntl.bufr_log, "big" );     break;
          case MIDDLE_ENDIAN: 
            fprintf(BUFR_Cntl.bufr_log, "middle" );  break;
          default:            
            fprintf(BUFR_Cntl.bufr_log, "UNKNOWN" ); break;
        }
        fprintf(BUFR_Cntl.bufr_log, "-endian order\n" );
      }
#endif

      BM->LocalTable_Flag = 0;
      BM->Multiple_Msg_Flag = 0;
      BM->Compress_Flag = 0;
      /*
      * Initialize memory debugging linked list.  This list will be used
      * by BUFR_malloc() and BUFR_free() if MEMORY_DEBUG_PRINT is #define'd.
      */

      Mem_Head.beg_addr  = NULL;
      Mem_Head.end_addr  = NULL;
      Mem_Head.ref_count = 0;
      Mem_Head.next      = &Mem_Tail;
      Mem_Tail = Mem_Head;
    }
    
    if ( BUFR_Init_Sub( BI, BUFR_File,  ProcFlag) )
    {
      BUFR_Err_Log("BUFR_Init");
      return(1);
    }
    /*
    * Test if data type 11 (new table entries
    */
    if( BUFR_ProcType() == TYPE_DECODE &&
        BUFR_Msg.Info.DataCategory == 11)
    {
      s4bp_save = BM->Section4_Data.bp;
      if( Create_TableB())
      {
        BUFR_Err_Log( "BUFR_Read_Tables" );
        BUFR_Destroy(1);
        return 1;
      }
      BM->LocalTable_Flag = 1;
      /* 
	    * 050798 LAH: Test to see if message is to be
	    *    dumped (accessed).  If not,  reinitialize for next message.
	    *    Otherwise,  set pointers so that it can be dumped.
	    */
      if (BUFR_Cntl.Dump_Data_Type_11_Msg == NO )
      {
        if ( BUFR_Init( BI, BUFR_File,  ProcFlag) )
        {
          BUFR_Err_Log("BUFR_Init");
          fprintf(BUFR_Cntl.bufr_log,"Error on call to BUFR_Init_Sub after Type 11 message");
          return(1);
        }
      } else {
	      /* reset pointers so message can be dumped */
        BM->Section4_Data.bp = s4bp_save;
        BM->Section4_Data.byte_num = 0;
        BM->Section4_Data.bit_num = 0;
        BM->MsgStatus = BUFR_OK;
        BM->subset_index--;

/*  Added by VLP to free the exp_fxy_list before putting a new one in */
        FXY_List_Destroy(BM->exp_fxy_list);  
        BUFR_Decode();
      }
    }
    if ( BM->Compress_Flag == 1)D_Compression();
 
#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
    {
       fprintf(BUFR_Cntl.bufr_log, "<<< Exiting BUFR_Init\n");
    }
#endif

    return 0;
}
