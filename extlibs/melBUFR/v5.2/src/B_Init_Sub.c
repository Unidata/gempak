/*
 * BUFR_Init_Sub- VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Init_Sub - Used to Initialize BUFR message.
 *                 Return 1 on error, else 0.
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
 * 100897  LAH: Added uchar_t cast for BI->BUFR_Edition ~
 *              Added int cast for ftell ~
 * 101097  LAH: Made hex constants 32 bits
 * 102097  LAH: Added int cast for enum
 * 022498 LAH:  Added prints to bufr_log file.
 * 050798  LAH: Code extracted from B_Init and made into function
 * 100898  VLP: Fixed missing flag initialization for short
 *
 */

extern int BUFR_Size_of_File;

#include <time.h>
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Init_Sub( BUFR_Info_t* BI, char* BUFR_File, ProcFlag_t ProcFlag )

#else

int BUFR_Init_Sub( BI, BUFR_File, ProcFlag )
BUFR_Info_t* BI;            /* Contains requred BUFR information. */
char*        BUFR_File;     /* BUFR file to be processed. */
ProcFlag_t   ProcFlag;      /* Must be ENCODE or DECODE */

#endif
{
    extern BUFR_Msg_t   BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;
    BUFR_Msg_t* BM;

    char buf[128];

    int EOM_Flag, MM_Flag;

    BM = &BUFR_Msg;

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, ">>> Entering B_Init_Sub\n");
#endif


    MM_Flag = 0;
    /* 102097 LAH: Added int cast */
    EOM_Flag = (int) BM->FileStatus;
    MM_Flag = BM->Multiple_Msg_Flag;
    
    if(BUFR_Cntl.Missing_User_Set != 2){
      BM->Missing_Value = BI->MissingValue;
    } else {
      BM->Missing_Value = BUFR_Cntl.User_Missing_Value;
    }
    EOM_Flag = BM->LocalTable_Flag;

    /*
    * Initialize error message structure before anything else so that the
    * BUFR_Err_*() functions will work.
    */
    BUFR_Err_Init();

    /* Verify arguments. */

    if( BI == NULL )
    {
      BUFR_Err_Set( "BUFR_Init", "NULL BUFR_Info_t address" );
      fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Init", 
        "NULL BUFR_Info_t address" );
      return 1;
    }

    /* 092997  LAH: Added cast to correct Linux warning */
    if( BUFR_File == NULL || *BUFR_File == (char)NULL )
    {
        BUFR_Err_Set( "BUFR_Init", "NULL BUFR file name" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Init", 
	        "NULL BUFR file name" );
        return 1;
    }

    switch( ProcFlag )
    {
      case TYPE_ENCODE:
      case TYPE_DECODE:
        break;

      default:
        BUFR_Err_Set( "BUFR_Init",
           "ProcFlag_t value must be ENCODE or DECODE" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Init", 
	         "ProcFlag_t value must be ENCODE or DECODE" );
        return 1;
    }

    /* Initialize BUFR message structure */

    if( BM->InitStatus == BEEN_INITIALIZED )
    {
      /* User forgot to call BUFR_Destroy(). */
      BUFR_Destroy(0);

      if( ProcFlag == TYPE_ENCODE )
      {
        /* Zero out entire BUFR message structure. */
        (void) memset( (char*) BM, 0, sizeof(BUFR_Msg) );

        strcpy( BM->FileName, BUFR_File );
        BM->ProcFlag = ProcFlag;
        BM->MethFlag = METHOD_UNKNOWN;
        BM->Multiple_Msg_Flag = MM_Flag;
      }
    } else
    {
      strcpy( BM->FileName, BUFR_File );
      BM->FilePtr  = NULL;
      BM->ProcFlag = ProcFlag;
      BM->MethFlag = METHOD_UNKNOWN;
    }

    /* Initialize stacks used for Table C operators. */

    if( ValStack_Init( &BM->DataWidthStack ) )
    {
      BUFR_Err_Log( "BUFR_Init" );
      return 1;
    }

    if( ValStack_Init( &BM->ScaleStack ) )
    {
      BUFR_Err_Log( "BUFR_Init" );
      return 1;
    }

    /* Initialize linked list of associated field values. */

    if( (BM->af_list=(AF_List_t*)malloc( sizeof(AF_List_t) )) == NULL )
    {
      BUFR_Err_Set( "BUFR_Init", "Can't create associated field list" );
      fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Init", 
	        "Can't create associated field list" );
      return 1;
      } else if ( AF_List_Init( BM->af_list ) )
      {
        BUFR_Err_Log( "BUFR_Init" );
        return 1;
      }

      /* Initialize linked list of data values (TYPE_ENCODE). */

      BM->data_list = (DataList_t*) malloc( sizeof(DataList_t) );

      if( BM->data_list == NULL )
      {
        BUFR_Err_Set( "BUFR_Init", "Can't create data list" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Init", 
	        "Can't create data list" );
        return 1;
      } else if( DataList_Init( BM->data_list ) )
      {
        BUFR_Err_Log( "BUFR_Init" );
        return 1;
      } else
      {
        BM->last_de = BM->data_list->head;      /* JRA021397 */
      }

      /*
      * Initialize expanded FXY array and subset counter used for decoding
      * messages or encoding templates.
      */

      BM->subset_index         = 1;
      BM->subset_size          = 0;
      BM->subset_fxys          = NULL;
      BM->exp_fxy_list         = NULL;
      BM->exp_ptr              = NULL;
      BM->rebuild_exp_fxy_list = 0;

      /**************************************/
      /* Initialize BUFR Section structures */
      /**************************************/

      memset( (char*)&BM->Section0, 0, sizeof( Section0_t ) );
      memset( (char*)&BM->Section1, 0, sizeof( Section1_t ) );
      memset( (char*)&BM->Section2, 0, sizeof( Section2_t ) );
      memset( (char*)&BM->Section3, 0, sizeof( Section3_t ) );
      memset( (char*)&BM->Section4, 0, sizeof( Section4_t ) );
      memset( (char*)&BM->Section5, 0, sizeof( Section5_t ) );

      BM->Section0.message_length = IntToInt3( 0 );
      /* 100897 LAH: Added uchar_t cast */
      BM->Section0.edition        = (uchar_t) BI->BUFR_Edition;

      BM->Section1.length = IntToInt3( 0 );
      BitStream_Init( &BM->Section1_Data, 0 );

      BM->Section2.length   = IntToInt3( 0 );
      BM->Section2.reserved = 0;
      BitStream_Init( &BM->Section2_Data, 0 );

      BM->Section3.length   = IntToInt3( 0 );
      BM->Section3.reserved = 0;
      BitStream_Init( &BM->Section3_Data, 0 );

      BM->Section4.length   = IntToInt3( 0 );
      BM->Section4.reserved = 0;
      BitStream_Init( &BM->Section4_Data, 0 );

      if( ProcFlag == TYPE_DECODE )
      {
        /*
        * Read the BUFR message, to get the BUFR_Info_t information from the
        * BUFR message file, in order to determine which tables to read.
        */

        if( BM->InitStatus != BEEN_INITIALIZED )
        {
#ifdef _WIN32
          if( (BM->FilePtr = fopen( BM->FileName, "rb" )) == NULL )
#else
          if( (BM->FilePtr = fopen( BM->FileName, "r" )) == NULL )
#endif
          {
            sprintf( buf, "%s: Can't open file for reading",BM->FileName);
            BUFR_Err_Set( "BUFR_Init", buf );
            fprintf(BUFR_Cntl.bufr_log, "%s\n", buf);
            BUFR_Destroy(1);
            return 1;
          } else
          {
            BM->FileStatus = BUFR_OK;
            BM->MsgStatus  = BUFR_OK;

            /*  Find size of file */
            fseek(BM->FilePtr, 0, SEEK_END);
            /* 100897 LAH: added int cast */
            BUFR_Size_of_File = (int) ftell(BM->FilePtr);
            fseek(BM->FilePtr, 0, SEEK_SET);

          }

          /*
          * Prepare for the call to BUFR_Read_Msg() by positioning the
          * file pointer to the beginning of the BUFR message.
          */
/*
          if( BUFR_Position_Msg() )
          {
            sprintf( buf, "%s: File is not a BUFR message",
                 BM->FileName );
            BUFR_Err_Set( "BUFR_Init_Sub", buf );
		        fprintf(BUFR_Cntl.bufr_log, "%S\n", buf);
            BUFR_Destroy();
            BUFR_Close();
            BUFR_Destroy();
            return 1;
          }
*/
        }

        if( BUFR_Position_Msg() )
        {
          if ( BUFR_Cntl.num_messages == 0 )
          {
            sprintf( buf, "%s: File is not a BUFR message",
               BM->FileName );
/*            BUFR_Err_Set( "BUFR_Init_Sub", buf );  */
            fprintf(stderr, "%s\n", buf);
            fprintf(BUFR_Cntl.bufr_log, "%s\n", buf);
          } else {
            sprintf( buf, "%s: At least one BUFR Message has been processed.\n", 
                BM->FileName );
            strcat(buf, "       There appears to be extra bytes at the end\n");
/*            BUFR_Err_Set( "BUFR_Init_Sub", buf );  */
            fprintf(stderr, "%s\n", buf);	      
            fprintf(BUFR_Cntl.bufr_log, "%s\n", buf);	      
          }
          BUFR_Destroy(0);
          BUFR_Close();
          BUFR_Destroy(1);
          return 1;
        }
        /* Read BUFR message to get information structure. */
        BUFR_Cntl.num_messages++;
	
        if( BUFR_Read_Msg() )
        {
          BUFR_Err_Log( "BUFR_Init_Sub" );
          BUFR_Close();
          BUFR_Destroy(1);
          return 1;
        }
        if(Int3ToInt(BM->Section4.length) == 4)
        {
          BM->FileStatus = BUFR_EOM;
          return 0;
        }
      } else if( ProcFlag == TYPE_ENCODE )
      {
        /* Store calling function's BUFR_Info_t data. */

        BM->Info = *BI;
      }

      /**************************************/
      /* Read BUFR master and local tables. */
      /**************************************/

      if(!EOM_Flag){
      /* Initialize table names to NULL  */

      BM->MasterTable0 = NULL;
      BM->MasterTableA = NULL;
      BM->MasterTableB = NULL;
      BM->MasterTableD = NULL;

      BM->LocalTableB = NULL;
      BM->LocalTableD = NULL;

      /* Initialize table structures. */

      Table0_Init();
      TableA_Init();

      if( TableB_Init() )
      {
        BUFR_Err_Log( "BUFR_Init_Sub" );
        BUFR_Destroy(1);
        return 1;
      }

      if( TableD_Init() )
      {
        BUFR_Err_Log( "BUFR_Init_Sub" );
        BUFR_Destroy(1);
        return 1;
      }

      /* Read Master and (possibly) local tables. */

      if( BUFR_Read_Tables() )
      {
        BUFR_Err_Log( "BUFR_Init_Sub" );
        BUFR_Close();
        BUFR_Destroy(1);
        return 1;
      }
    }

    if( ProcFlag == TYPE_DECODE )
    {
      /* Convert Section 3 and 4 data to bitstreams. */

      if( BUFR_Decode() )
      {
        BUFR_Err_Log( "BUFR_Init_Sub" );
        BUFR_Close();
        BUFR_Destroy(1);
        return 1;
      }

      /* Copy information read to calling function's BUFR_Info_t. */
      *BI = BM->Info;
    }

    BM->InitStatus = BEEN_INITIALIZED;


#ifdef TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "<<< Exiting B_Init_Sub\n");
#endif
    return 0;
}
