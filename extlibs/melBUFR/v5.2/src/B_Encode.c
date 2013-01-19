/*
 * BUFR_Encode - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

/* CHANGE LOG
*
* 100897  LAH: Added explicit casting to uchar_t line 39 & 43 to 59
* 012798  VLP: Added sub-center (byte 5 of section 1) to the structure
* 022498 LAH:  Added prints to bufr_log file.
* 100898  VLP: Check to see if compression is being done.  If it is, call the
*		function Compress.
*
*/
#if PROTOTYPE_NEEDED

int BUFR_Encode( BUFR_Info_t* BI )

#else

int BUFR_Encode( BI )
BUFR_Info_t* BI;

#endif
{

    BUFR_Msg_t* BM;

    int padded_values;
    double missing;  /*  Missing value variable */

    BM = &BUFR_Msg;
    missing = BM->Missing_Value;

    fprintf(BUFR_Cntl.bufr_log, "Entering BUFR_Encode\n");
    
    /* Copy the contents of the BUFR Information structure to the message. */

    if( BUFR_Info_Verify( *BI ) )
    {
        BUFR_Err_Log( "BUFR_Encode" );
        return 1;
    }

    BM->Info = *BI;

    BM->Section0.id[0] = 'B';
    BM->Section0.id[1] = 'U';
    BM->Section0.id[2] = 'F';
    BM->Section0.id[3] = 'R';

    /* 100897 LAH: Added u_char_t casts below */
    BM->Section1.master_table         = (uchar_t) BM->Info.BUFR_MasterTable;
    /*  must change short integer to integer structure to solve */
    /*  little/ big endian problem */
    /*  012798 VLP:  added sub-center to the structures */
    BM->Section1.sub_center	      = (uchar_t)BM->Info.SubCenter;
    BM->Section1.originating_center   = (uchar_t)BM->Info.OriginatingCenter;
    BM->Section1.update_sequence      = (uchar_t) BM->Info.UpdateSequenceNumber;
    BM->Section1.data_category        = (uchar_t) BM->Info.DataCategory;
    BM->Section1.data_sub_category    = (uchar_t) BM->Info.DataSubCategory;
    BM->Section1.master_table_version = (uchar_t) BM->Info.VersionNumberOfMasterTables;
    BM->Section1.local_table_version  = (uchar_t) BM->Info.VersionNumberOfLocalTables;
    BM->Section1.year                 = (uchar_t) BM->Info.Year;
    BM->Section1.month                = (uchar_t) BM->Info.Month;
    BM->Section1.day                  = (uchar_t) BM->Info.Day;
    BM->Section1.hour                 = (uchar_t) BM->Info.Hour;
    BM->Section1.minute               = (uchar_t) BM->Info.Minute;

    BM->Section3.flags += (uchar_t) (BI->ObservedData << (BITS_IN_BYTE-1));

    BM->Section5.id[0] = '7';
    BM->Section5.id[1] = '7';
    BM->Section5.id[2] = '7';
    BM->Section5.id[3] = '7';

    /*
     * Make sure that the number of data subsets is correct prior to calling
     * DataList_Encode().  The number of data subsets must be at least 1.
     */

    if( BM->subset_index == 0 )
        BM->subset_index = 1;

    /*
     * If encoding templates, make sure that the current data subset is filled
     * out.  If it isn't full, fill the remainder of the subset with data
     * values of BM->Missing_Value and issue a warning message.
     */

    if( BUFR_ProcMethod() == METHOD_TEMPLATE )
    {
        /*
         * If the expanded FXY pointer is at the start of the list, then the
         * dataset has been completely processed.  If so, decrement the subset
         * index.  If not, scan the remainder of the dataset for
         */

        if( BM->exp_ptr == FXY_List_First( BM->exp_fxy_list ) )
        {
            BM->subset_index--;
        }
        else
        {
            padded_values = 0;

            while( BM->exp_ptr != BM->exp_fxy_list->tail )
            {
                if( FXY_IsTableB( BM->exp_ptr->fxy ) )
                {
                    BUFR_Put_Value( BM->exp_ptr->fxy, missing );
                    padded_values++;
                }

                BM->exp_ptr = BM->exp_ptr->next;
            }

            if( padded_values > 0 )
            {
                fprintf( stderr, "WARNING: The remaining %d values ",
                    padded_values );
                fprintf( stderr, "in the dataset were not encoded.\n" );
                fprintf( stderr, "These values will be encoded as " );
                fprintf( stderr, "MISSING DATA to pad the BUFR message.\n" );
            }
        }
    }

    /*
     * For each data list element add descriptor to the Section 3 bit stream
     * and value(s) to Section 4 bit stream.
     */

#ifdef TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Encoding BUFR Message\n" );
#endif

    if( DataList_Encode() )
    {
        BUFR_Err_Log( "BUFR_Encode" );
        return 1;
    }
    if(Check_Compress()) Compression(); 

    if( BUFR_Write_Msg() )
    {
        BUFR_Err_Log( "BUFR_Encode" );
        return 1;
    }

    return 0;
}
