/*
 * BUFR_Info_Init - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Info_Init(): Initialize a BUFR_Infor_t structure with default values.
 * Members initialized as MISSING_VAL must be set prior to encoding.
 *
 * 012798  VLP:  Added default for sub-center
 * 022098  LAH: Added initialization of BUFR_Cntl structure 
 * 022398  LAH: Modified initialization of AutoFTP element by negation
 *              so that a test can be performed to se if user modifies
 *              the flag. 
 * 050798  LAH:  Added initialization for two new flags.  
 *               BUFR_Cntl.Print_New_Entries for printing new table entries
 *                      read from BUFR record
 *               BUFR_Cntl.Dump_Data_Type_11_Msg to dump message containing
 *                      new table entries.
 * 102104  STJ:  Set defaults for NCEP
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Info_Init( BUFR_Info_t* BI )

#else

int BUFR_Info_Init( BI )
BUFR_Info_t* BI;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    if( BI == NULL )
    {
        BUFR_Err_Set( "BUFR_Info_Init", "NULL BUFR_Info_t pointer" );
        return 1;
    }
    
    
    /* Master Table information. */

    BI->BUFR_Edition                = DEFAULT_BUFR_EDITION;
    BI->BUFR_MasterTable            = DEFAULT_MASTER_TABLE_NUMBER;
    BI->VersionNumberOfMasterTables = DEFAULT_MASTER_TABLE_VERSION_NUMBER;

    /* Local Table information. */

    BI->OriginatingCenter          = DEFAULT_ORIGINATING_CENTER;
    BI->SubCenter          	   = 0;
    BI->VersionNumberOfLocalTables = DEFAULT_LOCAL_TABLE_VERSION_NUMBER;
    BI->MinorLocalVersion          = DEFAULT_MINOR_VERSION_NUMBER;
    BI->GeneratingCenter           = GENERATING_CENTER_FLAG;

    BI->UpdateSequenceNumber = 0;           /* Original BUFR message. */
    BI->DataCategory         = 255; /* Must be set before encoding. */
    BI->DataSubCategory      = 0;
    /* LAH 112000 - added software version numbers */
    /* encoder puts this information in extra byts in section 1 */
    /* does not affect decoding by other software */
    BI->SoftVNum	     = 5;
    BI->SoftV2Num	     = 1;

    /*
     * The date and time for data in the BUFR message should be set prior to
     * calling BUFR_Encode().
     */

    BI->Year   = 0;
    BI->Month  = 0;
    BI->Day    = 0;
    BI->Hour   = 0;
    BI->Minute = 0;
    /* LAH 112000 - added century to info structure */
    /* note this only used by this software package and the century */
    /* is saved in the bytes reserved for local use.  Does not affect */
    /* decoding of message by other decoders. */
    BI->Century= 0;

    /*
     * Set Section 3, Octet 7 flag to indicate that data is non-compressed,
     * observed data (versus other data, such as forecast data).  This seems
     * to be a reasonable default value.
     */

    BI->ObservedData = 1;

    /*
     * JRA022697: If the AutoFTP flag is set, then fetch any missing local
     * tables via anonymous FTP.
     */
    /*
     * Negated flag to allow test to see if user modifies
     */
    BI->AutoFTP = -1;

     
/*  VLP052897:  Set the default missing data value */
 
    BI->MissingValue = BUFR_MISSING_VALUE;

    /* LAH022098  Added initialization of BUFR_Cntl structure */
    BUFR_Cntl.Auto_FTP = NO;
    BUFR_Cntl.Dup_Tab_Entry = YES;
    BUFR_Cntl.Dup_Tab_Entry_Warn = NO;
    BUFR_Cntl.BUFR_Log_Flag = YES;
    BUFR_Cntl.Print_New_Entries = NO;
    BUFR_Cntl.Dump_Data_Type_11_Msg = NO;
    BUFR_Cntl.Replace_Code_Table_Missing =  YES;
    BUFR_Cntl.Replace_Missing_Values = NO ;
    BUFR_Cntl.All_Numbers_Not_Double = YES;
    BUFR_Cntl.num_messages = 0;
    BUFR_Cntl.User_Missing_Value = 0;
    BUFR_Cntl.Missing_User_Set = 0;
    return 0;
}
