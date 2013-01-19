/*
 * dump_print - VERSION: %I%  %E% %T%
 *
 */
/*
 * dump_print(): Print information about a BUFR message.
 * The dataset numbers to print affect how much of Section 4 is displayed.
 * If both arguments are 0, everything is displayed.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         dump_print
 
  DESCRIPTION:         	dump_print(): Print information about a BUFR message.
 			The dataset numbers to print affect how much of 
			Section 4 is displayed.  If both arguments are 0, 
			everything is displayed.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     J. R. AKIN 
 
  CURRENT PROGRAMMER:  L. A. Hembree 
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
			void dump_print(interactive, first_dataset, 
					last_dataset, fp, *no_sets )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  interactive		Int	    IN		Flag for user prompting
  first_dataset		Int	    IN		First dataset to print
  last_dataset		Int	    IN		last dataset to print
  fp			FILE	    IN		File pointer
  no_sets               Int         OUT         Number of data sets in the
						message
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  first_data is bigger	Print error message.  Set dataset range to
  than last_dataset	the actual range
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Info_Print
  BUFR_NumDatasets	returns number of datasets in a message
  Table0_Value		Returns information about a given center
  TableA_Value		Returns information about data categories
  Int3ToInt		Makes an integer out of a 3 byte number
  FXY_IsTableD		Checks to see if a data descriptor is in Table D
  FXY_Expand		Expands out the FXY list
  TableB_Get		Gets Tables desciptors to match a FXY
  FXY_String		Unpacks a FXY and places into a string
  free		Frees malloc space
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME 	        TYPE    	DESCRIPTION
  ---- 	        ----    	-----------
  BUFR_Msg	BUFR_Msg_t	Bufr message structure
  BM		BUFR_Msg_t	Bufr message structure
  s0		Section0_t	Pointer to section 0 information
  s1		Section1_t	Pointer to section 1 information
  s2		Section2_t	Pointer to section 2 information
  s3		Section3_t	Pointer to section 3 information
  s4		Section4_t	Pointer to section 4 information
  s5		Section5_t	Pointer to section 5 information
  t0v		Table0_t	Table 0 information structure
  tav		TableA_t	Table A information structure
  fxy		FXY_t		FXY structure
  fxy_list	FXY_t		Pointer to the FXY structure
  num_fxys	int		Number of fxys in the data list
  f, n, i	int		loop counter
  DONE		int		loop done flag
  iyn		char		yes/no flag
  d		Descriptor_t	Table B structure
  ds_bytes	int		data secion 4 size
  num_ds	int		number of datasets
  first_byte	int		first byte of section 4 data
  last_byte	int		last byte of section 4 data
 
  METHOD:
	Initialize message pointer
	If user supplied file pointer is NULL then
	  File pointer equals standard out
	Perform BUFR_Info_Print
	Set first and last dataset number limits
	Set section pointers to the sections of the message
	Output section 0 and section 1
	If section 2 exists the output section 2
	Output section 3 descriptors list
	If any descriptors are Table D descriptors then
		Output expanded descriptors list
	Output section 4 information
	If user wishes data in hex format then
		Output section 4 data in hex format
	Output section 5

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    ????97 VLP: Modification to print replication message
    100997 LAH: Added int and double casts as indicated
    102097 LAH: LAH: Added ulong_t cast
    012798 VLP: Added sub-center to print
    030798 LAH: Modified to print to bufr_log file
    051998 LAH: Modified printing of fxys.  Created function FXY_List_Get
                to retrieve FXYs from Section 3,  used existing function 
		to expand FXYs and created function FXY_Print_Recursive to 
		print out expanded FXYs
		
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

void dump_print( int interactive, int first_dataset,
		 int last_dataset, FILE* fp, int *no_sets )

#else

void dump_print( interactive, first_dataset, last_dataset, fp, no_sets )
int   interactive;	/* Prompt user? */
int   first_dataset;    /* First dataset to print */
int   last_dataset;     /* Last  dataset to print */
FILE* fp;
int   *no_sets; /* number of data sets */

#endif
{

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

    FXY_t*       fxy_array;     /* Holds unexpanded S3 FXY values. */
    int          fxy_array_len;
    FXY_List_t*  exp_list;
    FXY_Entry_t* fxy_entry;
    
    int   DONE = 0;
    char  iyn;
    int   num_h_4;

    int ds_bytes, num_ds, first_byte, last_byte;

/*----------------------------------------------------------------------------*/

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
            "dump_print(): Invalid dataset range of %d to %d.\n", 
	      first_dataset, last_dataset);

        first_dataset = 1;
        last_dataset = BUFR_NumDatasets();

        fprintf( stderr, "Printing datasets %d to %d instead\n",
            first_dataset, last_dataset );
        fprintf(BUFR_Cntl.bufr_log,
            "dump_print(): Printing datasets %d to %d instead.\n", 
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
    fprintf( fp, "Originating Center:   %d\n", (int)s1->originating_center );
    fprintf( fp, "Sub-Center:           %d\n", (int)s1->sub_center );
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

    /* 100997 LAH: Added int cast */
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

#ifdef DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section1_Data.size , BM->Section1_Data.byte_num,
            BM->Section1_Data.bit_num );
    }

    if( BM->Section1_Data.size > 0 )
    {
        for( n=0; n < BM->Section1_Data.size; n++ )
        {
            if( (n % 16) == 0 )
                fprintf( BUFR_Cntl.bufr_log, "\n" );

            fprintf( BUFR_Cntl.bufr_log, "%02X ", BM->Section1_Data.buffer[n] );
        }
        fprintf( BUFR_Cntl.bufr_log, "\n" );
    }
#endif
    fprintf( fp, "\n" );

    fprintf( fp, "Section 2\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length: %d\n", Int3ToInt( s2->length ) );
    fprintf( fp, "Data Length:    %d\n", BM->Section2_Data.size );

#ifdef DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section2_Data.size, BM->Section2_Data.byte_num,
            BM->Section2_Data.bit_num );
    }
#endif

    if ( BM->Section2_Data.size > 0 ) {
	if ( interactive == 0 ) {
	    iyn = 'y';
	}
	else {
	    printf(" There are %d bytes to dump the data in hexidecimal format \n",
			BM->Section2_Data.size);
	    printf(" Do you wish a hex dump of section 2?  (y/n):  ");
	    while ( !DONE ) {
		scanf(" %c",&iyn);
		if ( iyn == 'y' || iyn == 'n' ) 
		    DONE = 1;
		else
		    printf(" wrong answer %c must be y or n ", iyn);
	    }
	    DONE = 0;
	}
      if( iyn == 'y')
      {
        for( n=0; n < BM->Section2_Data.size; n++ )
        {
          if( (n % 16) == 0 )
                fprintf( fp, "\n" );

          fprintf( fp, "%02X ", BM->Section2_Data.buffer[n] );
        }
        fprintf( fp, "\n" );
      }
    }
    fprintf( fp, "\n" );

    fprintf( fp, "Section 3\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length:               %d\n", 
        Int3ToInt( s3->length ) );
    fprintf( fp, "Number of Unexpanded FXYs:    %d\n", 
        (int)(BM->Section3_Data.size/2) );
    fprintf( fp, "Number of Data Sets:          %d\n", 
             Int2ToInt(s3->data_subsets) );
    fprintf( fp, "Flags:                        0x%02X [Data is %s and %s]\n"
	, s3->flags, ((s3->flags>>7) == 1 ? "observed" : "non-observed"),
        (((s3->flags>>6)&1) == 1 ? "compressed" : "non-compressed") );
    *no_sets = Int2ToInt(s3->data_subsets);

#ifdef DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
        fprintf( BUFR_Cntl.bufr_log, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
            BM->Section3_Data.size, BM->Section3_Data.byte_num,
            BM->Section3_Data.bit_num );
    }
#endif

    if( BM->Section3_Data.size > 0 )
    {
      /* 051998 LAH: code replaced by new function to retreivev FXYs
      *             Required so the file defined FXYs handled correctly.
      */
      fxy_array_len = FXY_List_Get(&fxy_array);
      fprintf( fp, "\n" );
      fprintf( fp, "Unexpanded Descriptors\n" );
      for( n=0; n < fxy_array_len;  n++ ) 
      {
        fprintf( fp, "%3d %4lX = %s\n", n, fxy_array[n],
               FXY_String(fxy_array[n]) );
      }

      fprintf( fp, "\nExpanded Descriptors\n" );
      /* 051998 LAH: Code replace by call to FXY_List_Expand  */
      exp_list = FXY_List_Expand( fxy_array, fxy_array_len );
      fxy_entry = exp_list->head->next;

	    fprintf( fp, "\n" );
      for( n=0; n < exp_list->num_fxys; n++ ) 
      {
        fprintf(fp, "%3d ", n);
	      FXY_Print_Recursive(fxy_entry->fxy, fp);
	      /* 051998 LAH code modified and moved to FXY_Print_Recursice */
        fxy_entry = fxy_entry->next;
      }

      fprintf( fp, "\n" );
    }

    fprintf( fp, "\n" );

    fprintf( fp, "Section 4\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "Section Length: %d\n", Int3ToInt( s4->length ) );
    fprintf( fp, "Data Length:    %d\n", BM->Section4_Data.size );


#ifdef DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
      fprintf( fp, "BitStream: Length=%d, Byte=%d, Bit=%d\n",
          BM->Section4_Data.size, BM->Section4_Data.byte_num,
          BM->Section4_Data.bit_num );
    }
#endif

    if( BM->Section4_Data.size > 0 )
    {
      fprintf( fp, "\n" );

      ds_bytes = BM->Section4_Data.size;
      num_ds   = BUFR_NumDatasets();

      if( first_dataset > 1 )
      {
        /* 100997 LAH: Added int cast */
        first_byte = (int) ((double) ds_bytes / (double) num_ds
                     * (double) (first_dataset-1));
      } else
        first_byte = 0;

      if( last_dataset < num_ds )
      {
        /* 100997 LAH: Added int and double cast on last line */
        /* does this calculation really need to be double??? */
        last_byte = (int) ((double) ds_bytes / (double) num_ds
                    * (double) last_dataset
                    + (double) ((ds_bytes % num_ds) != 0 ));
      } else
        last_byte = ds_bytes-1;

	if ( interactive == 0 ) {
	    iyn = 'n';
	}
	else {
	    printf(" Section 4 is %d bytes long\n", last_byte);
	    printf(" Do you wish a hex dump of section 4?  (y/n):  ");
	    while ( !DONE ) {
		scanf(" %c",&iyn);
		if ( iyn == 'y' || iyn == 'n' ) 
		    DONE = 1;
		else
		    printf(" wrong answer %c must be y or n ", iyn);
	    }
	    DONE = 0;
	}
	if ( num_ds > 0 && iyn == 'y' ) {
	    if ( num_ds > 1000 ) {
          printf(" Since there is over 1000 bytes of data, do you want ");
          printf("\nthe whole data set or just the first 1000 bytes? \n");
          printf("\n\n The first 1000 bytes (y/n):  ");
          while(!DONE)
          {
            scanf(" %c",&iyn);
            if(iyn == 'y' || iyn == 'n') 
               DONE = 1;
            else
               printf(" wrong answer %c must be y or n ", iyn);
          }
          DONE = 0;
          num_h_4 = last_byte;;
          if(iyn == 'y') 
              num_h_4 = 1000;
        } else
           num_h_4 = last_byte;

        if( first_dataset != last_dataset )
        {
          fprintf( fp, "Dump of datasets %d to %d, bytes %d to %d\n",
                 first_dataset, last_dataset, first_byte, num_h_4 );
        } else
        {
          fprintf( fp, "Dump of dataset %d, bytes %d to %d\n",
                    first_dataset, first_byte, num_h_4 );
        }

        for( n=first_byte; n <= num_h_4; n++ )
        {
          if( ((n-first_byte) % 16) == 0 )
               fprintf( fp, "\n" );

          fprintf( fp, "%02X ", BM->Section4_Data.buffer[n] );
        }
        fprintf( fp, "\n" );
      }

    }
    fprintf( fp, "\n" );

    fprintf( fp, "Section 5\n" );
    fprintf( fp, "---------\n" );
    fprintf( fp, "ID: %c%c%c%c\n", s5->id[0],s5->id[1],s5->id[2],s5->id[3] );

    PrintDivider( '-', 78, fp );

    fflush( fp );
}
