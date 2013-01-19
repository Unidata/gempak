/*
 * char* - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Info_Print(): Print contents of a BUFR information structure.
 */

/*
 * Change LOG 
 *
 * 092997  LAH: Added char cast to correct Linux warning 
 * 102097  LAH: Added double cast
 * 012798  VLP: Added sub-center (byte 5 of section 1) to prints
 * 111604  ML:  1 -> BITS_IN_BYTE-1 at "Optional Data: " section
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED
static char* ival( int val )
#else
static char* ival( val )
int val;
#endif
{
    static char str[64];

    /* 102097 LAH: Added double cast */
    if( (double) val == BUFR_MISSING_VALUE )
        sprintf( str, "[MISSING VALUE]" );
    else
        sprintf( str, "%d", val );

    return str;
}

#if PROTOTYPE_NEEDED
static char* sval( char* val )
#else
static char* sval( val )
char* val;
#endif
{
    static char str[64];

    /* 092997  LAH: Added cast to correct Linux warning */
    if( val == NULL || *val == (char) NULL )
        sprintf( str, "[NULL]" );
    else
        sprintf( str, "\"%s\"", val );

    return str;
}

#if PROTOTYPE_NEEDED

void BUFR_Info_Print( BUFR_Info_t BI, FILE* fp )

#else

void BUFR_Info_Print( BI, fp )
BUFR_Info_t BI;
FILE*       fp;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;

    BUFR_Msg_t* BM;

    Table0_t t0v;
    TableA_t tav;

    if( fp == NULL )
        fp = stdout;

    PrintDivider( '-', 78, fp );

    fprintf( fp, "BUFR Information Structure Contents\n" );
    fprintf( fp, "\n" );

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        /* Print information about the BUFR message being decoded. */

        BM = &BUFR_Msg;

        fprintf( fp, "BUFR File Name:       %s\n", sval(BM->FileName) );
        fprintf( fp, "Total Message Length: %d bytes\n",
            Int3ToInt( BM->Section0.message_length ) );
        fprintf( fp, "Number Data Subsets:  %d\n", 
                Int2ToInt(BM->Section3.data_subsets) );
        /* Int2ToInt add 10June 1997 - Hembree */
        fprintf( fp, "Is Data Observed?     %s\n",
            ( (BM->Section3.flags >> (BITS_IN_BYTE-1)) & 1 ? "Yes" : "No") );

        fprintf( fp, "Is Data Compressed?   %s\n",
            ( (BM->Section3.flags >> (BITS_IN_BYTE-2)) & 1 ? "Yes" : "No") );

        fprintf( fp, "Optional Data:        " );

        if( (BM->Section1.flags&(1<<(BITS_IN_BYTE-1))) != 0 )
        {
            fprintf( fp, "%d bytes\n", Int3ToInt( BM->Section2.length ) );
        }
        else if( Int3ToInt( BM->Section2.length ) != 0 )
        {
            fprintf( fp, "Flag not set but %d bytes are in Section 2!\n",
                Int3ToInt( BM->Section2.length ) );
        }
        else
        {
            fprintf( fp, "No optional data present\n" );
        }
    }

    fprintf( fp, "BUFR Edition:         %s\n", ival( BI.BUFR_Edition ) );

    fprintf( fp, "Master Table:         %s\n",
			ival( BI.BUFR_MasterTable ) );
    fprintf( fp, "  Version:            %s\n",
        		ival( BI.VersionNumberOfMasterTables ) );

    fprintf( fp, "Originating Center:   %s", ival( BI.OriginatingCenter ) );

    /* 102097 LAH: Added double cast */
    if( (double) BI.OriginatingCenter != BUFR_MISSING_VALUE )
    {
        t0v = Table0_Value( BI.OriginatingCenter );

        if( t0v.name == NULL )
            fprintf( fp, " [UNKNOWN CENTER ID]\n" );
        else
            fprintf( fp, " [%s]\n", t0v.name );
    }
    else
        fprintf( fp, "\n" );

    fprintf( fp, "Generating Center:    %s", ival( BI.GeneratingCenter ) );

    /* 102097 LAH: Added double cast */
    if( (double) BI.GeneratingCenter != BUFR_MISSING_VALUE)
    {
        t0v = Table0_Value( BI.GeneratingCenter );

        if( t0v.name == NULL )
            fprintf( fp, " [UNKNOWN CENTER ID]\n" );
        else
            fprintf( fp, " [%s]\n", t0v.name );
    } else
        fprintf( fp, "\n" );

    fprintf( fp, "Local Table Version:  %s",
        ival( BI.VersionNumberOfLocalTables ) );

    if( BI.MinorLocalVersion )
        fprintf( fp, ", Minor Version %s\n", ival((int)BI.MinorLocalVersion) );
    else
        fprintf( fp, "\n" );

    fprintf( fp, "Update Sequence #:    %s", ival(BI.UpdateSequenceNumber) );

    if( BI.UpdateSequenceNumber == 0 )
        fprintf( fp, " [Original Message]\n" );
    else
        fprintf( fp, "\n" );

    fprintf( fp, "Data Category:        %s", ival( BI.DataCategory ) );

    /* 102097 LAH: Added double cast */
    if( (double) BI.DataCategory != BUFR_MISSING_VALUE)
    {
        if( (tav=TableA_Value( BI.DataCategory )) == NULL )
            fprintf( fp, " [UNKNOWN]\n" );
        else
            fprintf( fp, " [%s]\n", tav );
    }

    fprintf( fp, "Data Sub-category:    %s\n", ival( BI.DataSubCategory ) );

    fprintf( fp, "Month:                %s\n", ival( BI.Month ) );
    fprintf( fp, "Day:                  %s\n", ival( BI.Day ) );
    fprintf( fp, "Year of Century       %s\n", ival( BI.Year ) );

    fprintf( fp, "Time:                 %s", ival( BI.Hour ) );
    fprintf( fp, ":%s\n", ival( BI.Minute ) );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() && BUFR_ProcType() == TYPE_DECODE )
    {
        /* Print BUFR Table file names. */

        fprintf( fp, "Master Table 0: %s\n", sval( BM->MasterTable0 ) );
        fprintf( fp, "Master Table A: %s\n", sval( BM->MasterTableA ) );
        fprintf( fp, "Master Table B: %s\n", sval( BM->MasterTableB ) );
        fprintf( fp, "Master Table D: %s\n", sval( BM->MasterTableD ) );

        fprintf( fp, "Local Table B:  %s\n", sval( BM->LocalTableB ) );
        fprintf( fp, "Local Table D:  %s\n", sval( BM->LocalTableD ) );
    }

#endif
    PrintDivider( '-', 78, fp );
}
