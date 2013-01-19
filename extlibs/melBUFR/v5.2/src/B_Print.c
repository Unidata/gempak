/*
 * BUFR_Print - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Print(): Print information about a BUFR message.
 * The dataset numbers to print affect how much of Section 4 is displayed.
 * If both arguments are 0, everything is displayed.
 */
/*
 * CHANGE LOG
 *
 * 100897 LAH: Added int cast
 * 100997 LAH: Added int cast
 * 102097 LAH: Added ulong_t cast
 * 012698 VLP: Split originating center into sub-center and originating center
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Print( int first_dataset, int last_dataset, FILE* fp )

#else

void BUFR_Print( first_dataset, last_dataset, fp )
int   first_dataset;    /* First dataset to print */
int   last_dataset;     /* Last  dataset to print */
FILE* fp;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    BUFR_Msg_t* BM;

    Section0_t* s0;
    Section1_t* s1;
    Section2_t* s2;
    Section3_t* s3;
    Section4_t* s4;
    Section5_t* s5;

    int n;

    Table0_t t0v;
    TableA_t tav;

    FXY_t fxy, *fxy_list;
    int   num_fxys, f;

    int ds_bytes, num_ds, first_byte, last_byte;

    BM = &BUFR_Msg;

    if( fp == NULL )    /* Use standard output */
        fp = stdout;

    /* Print contents of BUFR information structure. */

    BUFR_Info_Print( BM->Info, fp );

    if( first_dataset < 1 )
        first_dataset = 1;

    if( last_dataset < 1 )
        last_dataset = BUFR_NumDatasets();

    if( last_dataset < first_dataset )
    {
        fprintf( stderr, "BUFR_Print(): Invalid dataset range of %d to %d.\n",
            first_dataset, last_dataset );
        fprintf(BUFR_Cntl.bufr_log,
            "BUFR_Print(): Invalid dataset range of %d to %d.\n", 
	      first_dataset, last_dataset);

        first_dataset = 1;
        last_dataset = BUFR_NumDatasets();

        fprintf( stderr, "Printing datasets %d to %d instead\n",
            first_dataset, last_dataset );
        fprintf(BUFR_Cntl.bufr_log,
            "BUFR_Print(): Printing datasets %d to %d instead.\n", 
	      first_dataset, last_dataset);

        fflush( stderr );
    }

    s0 = &BM->Section0;
    s1 = &BM->Section1;
    s2 = &BM->Section2;
    s3 = &BM->Section3;
    s4 = &BM->Section4;
    s5 = &BM->Section5;

    fprintf( fp, "BUFR Message Contents\n" );
    fprintf( fp, "\n" );

    fprintf( fp, "Section 0\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "ID:             %c%c%c%c\n", s0->id[0], s0->id[1],
        s0->id[2], s0->id[3] );
    fprintf( fp, "Message Length: %d\n", Int3ToInt( s0->message_length ) );
    fprintf( fp, "BUFR Edition:   %d\n", s0->edition);
    fprintf( fp, "\n" );

    fprintf( fp, "Section 1\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length:       %d\n", Int3ToInt( s1->length ) );
    fprintf( fp, "Data Length:          %d\n", BM->Section1_Data.size );
    fprintf( fp, "BUFR Master Table:    %d [%s]\n", s1->master_table,
        (s1->master_table == 0 ? "Standard WMO FM 94" : "Unknown") );
    t0v = Table0_Value( (int)s1->originating_center );
    fprintf( fp, "Originating Center:   %d", (int)s1->originating_center );
    fprintf( fp, "Sub-Center:           %d", (int)s1->sub_center );
    if( t0v.name != NULL )
        fprintf( fp, " [%s]\n", t0v.name );
    else
        fprintf( fp, "\n" );
    fprintf( fp, "Update Sequence #:    %d [%s BUFR Message]\n",
        s1->update_sequence,
        (s1->update_sequence == 0 ? "Original" : "Updated" ) );
    fprintf( fp, "Optional Data:        %d [%s data present]\n",
        (s1->flags&1), ((s1->flags&1) == 1 ? "Optional" : "No optional") );
    fprintf( fp, "Data Category:        %d", s1->data_category );
    /* 100897 LAH:  Added int cast */
    tav = TableA_Value( (int) s1->data_category );

    if( tav != NULL )
        fprintf( fp, " [%s]\n", tav );
    else
        fprintf( fp, "\n" );

    fprintf( fp, "Data Sub Category:    %d\n", s1->data_sub_category );
    fprintf( fp, "Master Table Version: %d\n", s1->master_table_version );
    fprintf( fp, "Local  Table Version: %d\n", s1->local_table_version );
    fprintf( fp, "Date:                 %02d/%02d/%02d\n",
        s1->month, s1->day, s1->year );
    fprintf( fp, "Time:                 %02d:%02d\n", s1->hour, s1->minute );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section1_Data.size , BM->Section1_Data.byte_num,
            BM->Section1_Data.bit_num );
    }
#endif

    if( BM->Section1_Data.size > 0 )
    {
        for( n=0; n < BM->Section1_Data.size; n++ )
        {
            if( (n % 16) == 0 )
                fprintf( fp, "\n" );

            fprintf( fp, "%02X ", BM->Section1_Data.buffer[n] );
        }
        fprintf( fp, "\n" );
    }
    fprintf( fp, "\n" );

    fprintf( fp, "Section 2\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length: %d\n", Int3ToInt( s2->length ) );
    fprintf( fp, "Data Length:    %d\n", BM->Section2_Data.size );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section2_Data.size, BM->Section2_Data.byte_num,
            BM->Section2_Data.bit_num );
    }
#endif

    if( BM->Section2_Data.size > 0 )
    {
        for( n=0; n < BM->Section2_Data.size; n++ )
        {
            if( (n % 16) == 0 )
                fprintf( fp, "\n" );

            fprintf( fp, "%02X ", BM->Section2_Data.buffer[n] );
        }
        fprintf( fp, "\n" );
    }
    fprintf( fp, "\n" );

    fprintf( fp, "Section 3\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length: %d\n", Int3ToInt( s3->length ) );
    fprintf( fp, "Data Length:    %d\n", BM->Section3_Data.size );
    /*  Int2ToInt add 10 June 1997 - Hembree */
    fprintf( fp, "Data Subsets:   %d\n", Int2ToInt(s3->data_subsets) );
    fprintf( fp, "Flags:          0x%02X [Data is %s and %s]\n", s3->flags,
        ((s3->flags>>7) == 1 ? "observed" : "non-observed"),
        (((s3->flags>>6)&1) == 1 ? "compressed" : "non-compressed") );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section3_Data.size, BM->Section3_Data.byte_num,
            BM->Section3_Data.bit_num );
    }
#endif

    if( BM->Section3_Data.size > 0 )
    {
        fprintf( fp, "\n" );

        for( n=0; n < (BM->Section3_Data.size-1); n+=2 )
        {
            fprintf( fp, "%02X%02X", BM->Section3_Data.buffer[n],
                BM->Section3_Data.buffer[n+1] );

            /* 102097 LAH: Added ulong_t cast */
            fxy = (FXY_t) (BM->Section3_Data.buffer[n] << BITS_IN_BYTE)
                |  (ulong_t) BM->Section3_Data.buffer[n+1];

            if( FXY_IsTableD( fxy ) )
            {
                fprintf( fp, " = %s = ", FXY_String( fxy ) );

                if( (num_fxys=FXY_Expand( fxy, &fxy_list )) > 0 )
                {
                    for( f=0; f < num_fxys; f++ )
                    {
                        if( f != 0 && (f % 6) == 0 )
                            fprintf( fp, "\n                  " );

                        fprintf( fp, "%s ", FXY_String( fxy_list[f] ) );
                    }

                    if( (f-1) % 6 != 0 )
                        fprintf( fp, "\n" );

                     free( (void*) fxy_list );
                } else
                    fprintf( fp, "??? (Cannot expand FXY value)\n" );
            } else
                fprintf( fp, " = %s\n", FXY_String( fxy ) );
        }

        fprintf( fp, "\n" );
    }

    fprintf( fp, "\n" );

    fprintf( fp, "Section 4\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length: %d\n", Int3ToInt( s4->length ) );
    fprintf( fp, "Data Length:    %d\n", BM->Section4_Data.size );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section4_Data.size, BM->Section4_Data.byte_num,
            BM->Section4_Data.bit_num );
    }
#endif

    /*
     * Section 4 can be huge.
    if( BM->Section4_Data.size > 0 )
     *
     */

    if( BM->Section4_Data.size > 0 && BUFR_DebugLevel() > 1 )
    {
        fprintf( fp, "\n" );

        ds_bytes = BM->Section4_Data.size;
        num_ds   = BUFR_NumDatasets();

        if( first_dataset > 1 )
        {
            /* 100997 LAH: Added int cast */
            /* ??? does this calculation really need to be double */
            first_byte = (int) ((double) ds_bytes / (double) num_ds
                       * (double) (first_dataset-1));
        } else
            first_byte = 0;

        if( last_dataset < num_ds )
        {
            /* 100997 LAH: Added int cast */
            /* ??? does this calculation really need to be double */
            last_byte = (int) ((double) ds_bytes / (double) num_ds
                      * (double) last_dataset
                      + (double) ((ds_bytes % num_ds) != 0 ));
        } else
            last_byte = ds_bytes-1;

        if( num_ds > 1 )
        {
            if( first_dataset != last_dataset )
            {
                fprintf( fp, "Dump of datasets %d to %d, bytes %d to %d\n",
                    first_dataset, last_dataset, first_byte, last_byte );
            } else
            {
                fprintf( fp, "Dump of dataset %d, bytes %d to %d\n",
                    first_dataset, first_byte, last_byte );
            }
        }

        for( n=first_byte; n <= last_byte; n++ )
        {
            if( ((n-first_byte) % 16) == 0 )
                fprintf( fp, "\n" );

            fprintf( fp, "%02X ", BM->Section4_Data.buffer[n] );
        }

        fprintf( fp, "\n" );

    }
    fprintf( fp, "\n" );

    fprintf( fp, "Section 5\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "ID: %c%c%c%c\n", s5->id[0],s5->id[1],s5->id[2],s5->id[3] );

    PrintDivider( '-', 78, fp );

    fflush( fp );
}
