/*
 * BUFR_Read_Msg - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Read_Msg() - Read a BUFR message from the file referenced by
 * BUFR_Msg.FilePtr.  The file must be openened and positioned at the
 * beginning of the message.
 *
 * After successfully reading a BUFR message, try to position the file
 * pointer at the beginning of the next BUFR message.  If there are no
 * more BUFR messages in the file, set the status member of the BUFR
 * message structure to indicate EOF but DO NOT CLOSE THE FILE!
 */
/*
 * CHANGE LOG 
 *
 * 100897 LAH: Added int cast
 * 100997 LAH: Added size_t casts in fread           
 * 012897 VLP: Convert both the Originating center and the sub-center to
 *		integers and store in the BUFR Info structure
 * 022498 LAH:  Added prints to bufr_log file.
 * 092998 VLP:  Added Century of the year to octest 19 of Section 1.
 * 100898 VLP:  Added Compress flag fill.
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Read_Msg( void )

#else

int BUFR_Read_Msg()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    BUFR_Msg_t* BM;

    FILE* fp;

    char errbuf[1024];

    int any_s2_data, ns;

    char *s0p, *s1p, *s2p, *s3p, *s4p, *s5p;
    int   s0l,  s1l,  s2l,  s3l,  s4l,  s5l;

    char *s1dp, *s2dp, *s3dp, *s4dp;
    int  s1dl,  s2dl,  s3dl,  s4dl;
    int  i;
    char *tpr;
    int m_flag;
    char buf[80];

    Table0_t t0v;
    EncVal_t ev;
    double   d;
    BM = &BUFR_Msg;

    fp = BM->FilePtr;

    if( fp == NULL )
    {
        sprintf( buf, "File \"%s\" hasn't been opened for reading",
            BM->FileName );
        BUFR_Err_Set( "BUFR_Read_Msg", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Dataset", buf);
        return 1;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        printf( "Reading file %s\n", BM->FileName );
#endif

    s0p = (char*) &BM->Section0;
    s1p = (char*) &BM->Section1;
    s2p = (char*) &BM->Section2;
    s3p = (char*) &BM->Section3;
    s4p = (char*) &BM->Section4;
    s5p = (char*) &BM->Section5;

    /*
     * Due to alignment, there is an extra byte for Sections 1 and 3.
     * The sum of the sizes of the elements for Section1_t is 17 and
     * the sum of the sizes of the elements for Section3_t is  7.
     */

    s0l = sizeof( Section0_t );         /* Should be  8 */
/*    s1l = sizeof( Section1_t ) - 1;  */   /* Should be 17 */
/* structure changed to fix endian problem */
    s1l = sizeof( Section1_t );     /* Should be 17 */
    s2l = sizeof( Section2_t );         /* Should be  4 */
/*    s3l = sizeof( Section3_t ) - 1;  */   /* Should be  7 */
/* structure changed to fix endian problem */
    s3l = sizeof( Section3_t );     /* Should be  7 */
    s4l = sizeof( Section4_t );         /* Should be  4 */
    s5l = sizeof( Section5_t );         /* Should be  4 */

    /* Read section 0. */

    /* 100997 LAH: Added size_t cast */
    if( fread( s0p, 1, (size_t) s0l, fp ) != (size_t) s0l )
    {
        BUFR_Err_Set( "BUFR_Read_Msg", "Can't read Section 0" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg",
            "Can't read Section 0");
        return 1;
    }

    if( strncmp( BM->Section0.id, "BUFR", 4 ) != 0 )
    {
        sprintf( errbuf, "File \"%s\" is not a BUFR file", BM->FileName );
        BUFR_Err_Set( "BUFR_Read_Msg", errbuf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", errbuf);
        return 1;
    }

    /* Read section 1. */

    /* 100997 LAH: Added size_t cast */
    if( fread( s1p, 1, (size_t) s1l, fp ) != (size_t) s1l )
    {
        BUFR_Err_Set( "BUFR_Read_Msg", "Can't read Section 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
            "Can't read Section 1");
        return 1;
    } else
    {
      s1dl = Int3ToInt( BM->Section1.length ) - s1l;

      if( BitStream_Init( &BM->Section1_Data, s1dl ) )
         return 1;

      s1dp = (char*) BM->Section1_Data.buffer;

      /* 100997 LAH: Added size_t cast */
      if( fread( s1dp, 1, (size_t)s1dl, fp ) != (size_t)s1dl )
      {
        BUFR_Err_Set("BUFR_Read_Msg", "Can't read Section 1 bit stream");
        return 1;
      } else
      {
        BM->Section1_Data.bp = BM->Section1_Data.buffer;

/* check to see if there is additional data i.e. either Century or local table
  info at the end of section 1. */
        if(s1dl > 1){
        if( BitStream_Get( &BUFR_Msg.Section1_Data, &ev, BITS_IN_BYTE ) )
        {
          fprintf( stderr, "BUFR_Read_Msg: Can't get minor local " );
          fprintf( stderr, "version from Section 1, Octet 18\n" );
          fflush( stderr );
          BUFR_perror( "BUFR_Read_Msg" );
          BUFR_Err_Clear();
        }

        if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
        {
          fprintf( stderr, "BUFR_Read_Msg: Can't get minor local " );
          fprintf( stderr, "version or year from Section 1, Octet 18\n" );
          fflush( stderr );
          BUFR_perror( "BUFR_Read_Msg" );
          BUFR_Err_Clear();
        }
        /* LAH 112000 - free allocated memory */
        EncVal_Destroy( ev );
/* if d is less than 17 you have a minor local version number, now check for a
   generating center id */
        if(d < 17)
        {
          BM->Info.MinorLocalVersion = (uchar_t) d;
        	if( BitStream_Get( &BUFR_Msg.Section1_Data, &ev, 2*BITS_IN_BYTE ) )
         	{
            fprintf( stderr, "BUFR_Read_Msg: Can't get Generating " );
            fprintf( stderr, "Center from Section 1, Octet 19\n" );
            fflush( stderr );
            BUFR_perror( "BUFR_Read_Msg" );
            BUFR_Err_Clear();
         	}

        	if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
         	{
            fprintf( stderr, "BUFR_Read_Msg: Can't get Generating " );
            fprintf( stderr, "Center from Section 1, Octet 19\n" );
            fflush( stderr );
            BUFR_perror( "BUFR_Read_Msg" );
            BUFR_Err_Clear();
         	}
          BM->Info.GeneratingCenter = (uchar_t) d;
          /* LAH 112000 - free allocated memory */
          EncVal_Destroy( ev );
          } else
          {
            BM->Info.Century = (uchar_t) d;
            if(s1dl > 4)
            {
            	if( BitStream_Get( &BUFR_Msg.Section1_Data, &ev, BITS_IN_BYTE ) )
            	{
                fprintf( stderr, "BUFR_Read_Msg: Can't get Software " );
                fprintf( stderr, "Major Version from Section 1, Octet 19\n" );
                fflush( stderr );
                BUFR_perror( "BUFR_Read_Msg" );
                BUFR_Err_Clear();
            	}

            	if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
            	{
                fprintf( stderr, "BUFR_Read_Msg: Can't get Software " );
                fprintf( stderr, "Major Version from Section 1, Octet 19\n" );
                fflush( stderr );
                BUFR_perror( "BUFR_Read_Msg" );
                BUFR_Err_Clear();
            	}

              BM->Info.SoftVNum = (uchar_t) d;
              /* LAH 112000 - free allocated memory */
              EncVal_Destroy( ev );

            	if( BitStream_Get( &BUFR_Msg.Section1_Data, &ev, BITS_IN_BYTE ) )
            	{
                fprintf( stderr, "BUFR_Read_Msg: Can't get Software " );
                fprintf( stderr, "Minor Version from Section 1, Octet 20\n" );
                fflush( stderr );
                BUFR_perror( "BUFR_Read_Msg" );
                BUFR_Err_Clear();
            	}

            	if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
            	{
                fprintf( stderr, "BUFR_Read_Msg: Can't get Software " );
                fprintf( stderr, "Minor Version from Section 1, Octet 20\n" );
                fflush( stderr );
                BUFR_perror( "BUFR_Read_Msg" );
                BUFR_Err_Clear();
            	}
              BM->Info.SoftV2Num = (uchar_t) d;
		          /* LAH 112000 - free allocated memory */
              EncVal_Destroy( ev );

            	if( BitStream_Get( &BUFR_Msg.Section1_Data, &ev, BITS_IN_BYTE ) )
            	{
                fprintf( stderr, "BUFR_Read_Msg: Can't get Local Table Minor " );
                fprintf( stderr, "Version # from Section 1, Octet 21\n" );
                fflush( stderr );
                BUFR_perror( "BUFR_Read_Msg" );
                BUFR_Err_Clear();
            	}

            	if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
            	{
                fprintf( stderr, "BUFR_Read_Msg: Can't get Local Table Minor " );
                fprintf( stderr, "Version # from Section 1, Octet 21\n" );
                fflush( stderr );
                BUFR_perror( "BUFR_Read_Msg" );
                BUFR_Err_Clear();
            	}
              BM->Info.MinorLocalVersion = (uchar_t) d;
              /* LAH 112000 - free allocated memory */
              EncVal_Destroy( ev );
            }
          }
        }
      }
    }

    /* If there is any Section 2, read it. */

    any_s2_data = BM->Section1.flags >> (BITS_IN_BYTE-1);

    if( any_s2_data )
    {
      /* 100997 LAH: Added size_t cast */
      if( fread( s2p, 1, (size_t) s2l, fp ) != (size_t) s2l )
      {
        BUFR_Err_Set( "BUFR_Read_Msg", "Can't read Section 2" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
	         "Can't read Section 2");
        return 1;
        } else
        {
          s2dl = Int3ToInt( BM->Section2.length ) - s2l;

          if( BitStream_Init( &BM->Section2_Data, s2dl ) )
             return 1;

          s2dp = (char*) BM->Section2_Data.buffer;
          /* 100997 LAH: Added size_t cast */
          if( fread( s2dp, 1, (size_t) s2dl, fp ) != (size_t) s2dl )
          {
            BUFR_Err_Set( "BUFR_Read_Msg",
                "Can't read Section 2 bit stream" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
                "Can't read Section 2 bit stream");
            return 1;
          } else
            BM->Section2_Data.bp = BM->Section2_Data.buffer;
      }
    }

    /* Read section 3. */

    /* 100997 LAH: Added size_t cast */

    if( fread( s3p, 1, (size_t) s3l, fp ) != (size_t) s3l )
    {
      BUFR_Err_Set( "BUFR_Read_Msg", "Can't read Section 3" );
      fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
	       "Can't read Section 3");
      return 1;
    } else
    {
      s3dl = Int3ToInt( BM->Section3.length ) - s3l;

      if( BitStream_Init( &BM->Section3_Data, s3dl ) )
          return 1;

      s3dp = (char*) BM->Section3_Data.buffer;

      /* 100997 LAH: Added size_t cast */
      if( fread( s3dp, 1, (size_t) s3dl, fp ) != (size_t) s3dl )
      {
        BUFR_Err_Set("BUFR_Read_Msg", "Can't read Section 3 bit stream");
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
            "Can't read Section 3 bit stream");
        return 1;
      } else
        BM->Section3_Data.bp = BM->Section3_Data.buffer;
        /*  Int2ToInt add 10 June 1997 - Hembree */
        if( (ns = Int2ToInt(BM->Section3.data_subsets)) == 0 )
        {
          fprintf( stderr, "WARNING: # Data Subsets was 0, set to 1\n" );
          /*  Int2ToInt add 10 June 1997 - Hembree */
          BM->Section3.data_subsets = IntToInt2(1);
        }
      }

      /* Read section 4. */

      /* 100997 LAH: Added size_t cast */

      /*   if( (ns = fread( s4p, 1, (size_t) s4l, fp )) != (size_t) s4l ) */
      if( (ns = fread( s4p, 1, (size_t) s4l, fp )) != s4l )
      {
        BUFR_Err_Set( "BUFR_Read_Msg", "Can't read Section 4" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
	         "Can't read Section 4");
        return 1;
      } else
      {
        s4dl = Int3ToInt( BM->Section4.length ) - s4l;

        if( BitStream_Init( &BM->Section4_Data, s4dl ) )
            return 1;

        s4dp = (char*) BM->Section4_Data.buffer;

        /* 100997 LAH: Added size_t cast */

/*        if( (ns = (int) fread( s4dp, 1, (size_t) s4dl, fp )) != (size_t) s4dl ) */
        if( (ns = (int) fread( s4dp, 1, (size_t) s4dl, fp )) != s4dl )
        {
          tpr = s4dp;
          for ( i=0; i< ns; i++)
          {
            printf("%.4hx ", *tpr);
            tpr++;
            if ( (i % 10) == 0 )printf("\n");
          }
          BUFR_Err_Set("BUFR_Read_Msg", "Can't read Section 4 bit stream");
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
              "Can't read Section 4 bit stream");
          return 1;
        } else 
        {
/*          printf("ns = %d\n",ns);  */
          BM->Section4_Data.bp = BM->Section4_Data.buffer;
        }
    }

    /* Read section 5. */

    /* 100997 LAH: Added size_t cast */
    if( fread( s5p, 1, (size_t) s5l, fp ) != (size_t) s5l )
    {
        BUFR_Err_Set( "BUFR_Read_Msg", "Can't read Section 5" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Msg", 
            "Can't read Section 5");
        return 1;
    }

    if( strncmp( BM->Section5.id, "7777", 4 ) != 0 )
    {
        fprintf( stderr, "WARNING: Section 5 ID is %s instead of 7777\n",
            BM->Section5.id );
        fprintf(BUFR_Cntl.bufr_log,"WARNING: Section 5 ID is %s instead of 7777\n",
            BM->Section5.id );

/*****************************************************************************
        sprintf( errbuf, "File \"%s\" is not a BUFR file", BM->FileName );
        BUFR_Err_Set( "BUFR_Read_Msg", errbuf );
        return 1;
******************************************************************************/
    }

    /* Fill out BUFR information structure. */
    /* 100897 LAH: Added int casts below */
    BM->Info.BUFR_Edition                = (int) BM->Section0.edition;
    BM->Info.BUFR_MasterTable            = (int) BM->Section1.master_table;
    /* convert from Int2 structure to integer */
    BM->Info.SubCenter           	 = (int)BM->Section1.sub_center;
    BM->Info.OriginatingCenter           = (int)BM->Section1.originating_center;
    BM->Info.UpdateSequenceNumber        = (int) BM->Section1.update_sequence;
    BM->Info.DataCategory                = (int) BM->Section1.data_category;
    BM->Info.DataSubCategory             = (int) BM->Section1.data_sub_category;
    BM->Info.VersionNumberOfMasterTables = (int) BM->Section1.master_table_version;
    BM->Info.VersionNumberOfLocalTables  = (int) BM->Section1.local_table_version;
    BM->Info.Year                        = (int) BM->Section1.year;
    BM->Info.Month                       = (int) BM->Section1.month;
    BM->Info.Day                         = (int) BM->Section1.day;
    BM->Info.Hour                        = (int) BM->Section1.hour;
    BM->Info.Minute                      = (int) BM->Section1.minute;

    BM->Info.ObservedData = BM->Section3.flags>>(BITS_IN_BYTE-1);
    /* LAH 042900 added following line to include number of data sets */
    /* in BUFR info structure  */
    BM->Info.Num_Data_Sets = Int2ToInt(BM->Section3.data_subsets);
    if (((BM->Section3.flags>>6)&1) == 1) BM->Compress_Flag = 1;

    /*
     * If the version number of the local tables is not 0 and the originating
     * center supports minor version numbers for local tables (as indicated in
     * Table A), get the minor version number from Section 1, Octet 18.
     */

    if( BM->Info.VersionNumberOfLocalTables != 0 )
    {
      /*
      * See if originating center supports minor version numbers.
      * If t0v.name is NULL, the originating center isn't in Table A
      * so assume that the center does not use minor version numbers.
      */

      t0v = Table0_Value( BM->Info.OriginatingCenter );

      if( t0v.name != NULL && t0v.UsesLocalMinorVersion )
      {
        EncVal_Init( &ev );
        if( BitStream_Get( &BM->Section1_Data, &ev, BITS_IN_BYTE ) )
        {
          fprintf( stderr, "BUFR_Read_Msg: Can't get minor local " );
          fprintf( stderr, "version from Section 1, Octet 18\n" );
          fflush( stderr );
          fprintf( BUFR_Cntl.bufr_log,
            "BUFR_Read_Msg: Can't get minor local " );
          fprintf( BUFR_Cntl.bufr_log,
              "version from Section 1, Octet 18\n" );
          BUFR_perror( "BUFR_Read_Msg" );
          BUFR_Err_Clear();
        }

        if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
        {
          fprintf( stderr, "BUFR_Read_Msg: Can't get minor local " );
          fprintf( stderr, "version from Section 1, Octet 18\n" );
          fflush( stderr );
          fprintf( BUFR_Cntl.bufr_log, "BUFR_Read_Msg: Can't get minor local " );
          fprintf(BUFR_Cntl.bufr_log , "version from Section 1, Octet 18\n" );
          BUFR_perror( "BUFR_Read_Msg" );
          BUFR_Err_Clear();
        }

        BM->Info.MinorLocalVersion = (uchar_t) d;

        EncVal_Destroy( ev );

        /* JRA020497: Get generating center ID. */

        EncVal_Init( &ev );

        if( BitStream_Get( &BM->Section1_Data, &ev, 2*BITS_IN_BYTE ) )
        {
          /* Generating center ID isn't given. */
          BUFR_Err_Clear();
          BM->Info.GeneratingCenter = (int)BM->Missing_Value;
        } else if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
        {
          BUFR_Err_Clear();
          BM->Info.GeneratingCenter = BAD_VAL;
        } else
          BM->Info.GeneratingCenter = (int) d;
          EncVal_Destroy( ev );
        } else
        {
          BM->Info.MinorLocalVersion = 0;
          BM->Info.GeneratingCenter  = (int)BM->Missing_Value; /* JRA020497 */
        }
    } else
    {
      BM->Info.MinorLocalVersion = 0;
      BM->Info.GeneratingCenter  = (int)BM->Missing_Value;    /* JRA020497 */
    }

/*  050798 remove LAH
#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 7 )
        BUFR_Print( 0, 0, BUFR_Cntl.bufr_log );
#endif
*/
    /*
     * Position file pointer to next (possible) BUFR message.  Set status
     * to EOF if there are no more messages in the file and set it to EOM
     * if more messages remain in the file.
     */

    if( BUFR_Position_Msg() )
    {
        /* There are no more BUFR messages. */

        BUFR_Msg.FileStatus    = BUFR_EOF;
    } else
    {
        /* There are more BUFR messages remaining in this file. */

        BUFR_Msg.FileStatus    = BUFR_EOM;
    }

    BUFR_Msg.MsgStatus = BUFR_OK;

    return 0;
}
