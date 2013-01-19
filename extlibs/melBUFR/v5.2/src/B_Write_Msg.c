/*
 * BUFR_Write_Msg - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/*
 * CHANGE LOG
 *
 * ????97 VLP: Mods to handle writing of multiple messages to a file
 * 100997 LAH: Added int & size_t casts to fwrite 
 * 120897 VLP: Check total file size against maximum allowable file size
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#if PROTOTYPE_NEEDED

int BUFR_Write_Msg( void )

#else

int BUFR_Write_Msg()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;
    
    BUFR_Msg_t* BM;

    FILE* fp;
/*    int size;  */
    int n;

    int any_s2_data;

    char *s0p, *s1p, *s2p, *s3p, *s4p, *s5p;
    int   s0l,  s1l,  s2l,  s3l,  s4l,  s5l;

    char *s1dp, *s2dp, *s3dp, *s4dp;
    int   s1dl,  s2dl,  s3dl,  s4dl;

    int s1_len, s2_len, s3_len, s4_len;

    EncVal_t pad_ev;
    uchar_t  uc;

    char buf[128];

    BM = &BUFR_Msg;

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Writing to %s\n", BM->FileName );
#endif
/*  If the user is writing multiple messages, the file needs to be appended */
 
    if(BM->Multiple_Msg_Flag == 2) {
#ifdef _WIN32
      if( (fp = fopen( BM->FileName, "ab" )) == NULL )
#else
      if( (fp = fopen( BM->FileName, "a" )) == NULL )
#endif
      {
          sprintf( buf, "%s: Can't open file for writing", BM->FileName );
          BUFR_Err_Set( "BUFR_Write_Msg", buf );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", buf);
          return 1;
      }
    } else
    {
#ifdef _WIN32
      if( (fp = fopen( BM->FileName, "wb" )) == NULL )
#else
      if( (fp = fopen( BM->FileName, "w" )) == NULL )
#endif
      {
          sprintf( buf, "%s: Can't open file for writing", BM->FileName );
          BUFR_Err_Set( "BUFR_Write_Msg", buf );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", buf);
          return 1;
      }
    }

    /* Terminate bit streams */

    if( BitStream_Flush( &BM->Section1_Data ) )
        goto ABORT;

    if( BitStream_Flush( &BM->Section2_Data ) )
        goto ABORT;

    if( BitStream_Flush( &BM->Section3_Data ) )
        goto ABORT;

    if( BitStream_Flush( &BM->Section4_Data ) )
        goto ABORT;

    s0p = (char*) &BM->Section0;
    s1p = (char*) &BM->Section1;
    s2p = (char*) &BM->Section2;
    s3p = (char*) &BM->Section3;
    s4p = (char*) &BM->Section4;
    s5p = (char*) &BM->Section5;

    /*
     * Before the introduction of type Int2_t there was an extra byte
     * due to alignment for Sections 1 and 3. Hence the minus 1 in the
     * old length calculations below.
     * The sum of the sizes of the elements for Section1_t is 17 and
     * the sum of the sizes of the elements for Section3_t is  7.
     */

    s0l = sizeof( Section0_t );         /* Should be  8 */
    /* s1l = sizeof( Section1_t ) - 1; */    /* Should be 17 */
    s1l = sizeof( Section1_t );     /* Should be 17 */
    s2l = sizeof( Section2_t );         /* Should be  4 */
    /* s3l = sizeof( Section3_t ) - 1;  */   /* Should be  7 */
    s3l = sizeof( Section3_t );     /* Should be  7 */
    s5l = sizeof( Section5_t );         /* Should be  4 */
    s4l = sizeof( Section4_t );         /* Should be  4 */

    s1dp = (char*) BM->Section1_Data.buffer;
    s2dp = (char*) BM->Section2_Data.buffer;
    s3dp = (char*) BM->Section3_Data.buffer;
    s4dp = (char*) BM->Section4_Data.buffer;

    s1dl = BM->Section1_Data.bp - BM->Section1_Data.buffer;
    s2dl = BM->Section2_Data.bp - BM->Section2_Data.buffer;
    s3dl = BM->Section3_Data.bp - BM->Section3_Data.buffer;
    s4dl = BM->Section4_Data.bp - BM->Section4_Data.buffer;

    any_s2_data = BM->Section1.flags >> (BITS_IN_BYTE-1);

    if( any_s2_data == 0 )
    {
        s2l  = 0;
        s2dl = 0;
    }

    /*
     * Compute and store section lengths.  If necessary, force each section
     * length to be a multiple of 2 by adding a null byte to its bit stream.
     * NOTE: The bit stream pointers may have moved after padding.  Reset
     * pointers and lengths when padding.
     */

    s1_len = s1l + s1dl;
    s2_len = s2l + s2dl;
    s3_len = s3l + s3dl;
    s4_len = s4l + s4dl;

    uc           = 0;
    pad_ev.value = &uc;
    pad_ev.nbits = BITS_IN_BYTE;


    if( (s1_len % 2) != 0 )
    {
#if TRACE_PRINT
        if( BUFR_TraceLevel() > 1 )
            fprintf(BUFR_Cntl.bufr_log, "Padding Section 1\n" );
#endif

        if( BitStream_Put( &BM->Section1_Data, pad_ev ) )
            goto ABORT;
        else
        {
            s1dp = (char*) BM->Section1_Data.buffer;
            s1dl = BM->Section1_Data.bp - BM->Section1_Data.buffer;
            s1_len = s1l + s1dl;
        }
    }

    if( any_s2_data && (s2_len % 2) != 0 )
    {
#if TRACE_PRINT
        if( BUFR_TraceLevel() > 1 )
            fprintf(BUFR_Cntl.bufr_log, "Padding Section 2\n" );
#endif

        if( BitStream_Put( &BM->Section2_Data, pad_ev ) )
            goto ABORT;
        else
        {
            s2dp = (char*) BM->Section2_Data.buffer;
            s2dl = BM->Section2_Data.bp - BM->Section2_Data.buffer;
            s2_len = s2l + s2dl;
        }
    }

    if( (s3_len % 2) != 0 )
    {
#if TRACE_PRINT
        if( BUFR_TraceLevel() > 1 )
           fprintf(BUFR_Cntl.bufr_log, "Padding Section 3\n" );
#endif

        if( BitStream_Put( &BM->Section3_Data, pad_ev ) )
            goto ABORT;
        else
        {
            s3dp = (char*) BM->Section3_Data.buffer;
            s3dl = BM->Section3_Data.bp - BM->Section3_Data.buffer;
            s3_len = s3l + s3dl;
        }
    }

    if( (s4_len % 2) != 0 )
    {
#if TRACE_PRINT
        if( BUFR_TraceLevel() > 1 )
            fprintf(BUFR_Cntl.bufr_log, "Padding Section 4\n" );
#endif

        if( BitStream_Put( &BM->Section4_Data, pad_ev ) )
            goto ABORT;
        else
        {
            s4dp = (char*) BM->Section4_Data.buffer;
            s4dl = BM->Section4_Data.bp - BM->Section4_Data.buffer;
            s4_len = s4l + s4dl;
        }
    }

    n = s0l + s1_len + s2_len + s3_len + s4_len + s5l;

    /*  VLP:  Check to see if file to be written is too large */
    if(n > MAX_FILE_SIZE) 
    {
    	fprintf(BUFR_Cntl.bufr_log," Total file size is too large for BUFR  = %d \n", n);
	    fprintf(BUFR_Cntl.bufr_log," Maximum size allowed is:  %d \n", MAX_FILE_SIZE);
      fprintf(BUFR_Cntl.bufr_log," Section 1 Length = %d \n",s1_len); 
      fprintf(BUFR_Cntl.bufr_log," Section 2 Length = %d \n",s2_len); 
      fprintf(BUFR_Cntl.bufr_log," Section 3 Length = %d \n",s3_len); 
      fprintf(BUFR_Cntl.bufr_log," Section 4 Length = %d \n",s4_len); 
      fprintf(BUFR_Cntl.bufr_log," Section 5 Length = %d \n",s5l); 
      printf(" Total file size is too large for BUFR  = %d \n", n);
      printf(" Maximum size allowed is:  %d \n", MAX_FILE_SIZE);
      printf(" Section 1 Length = %d \n",s1_len); 
      printf(" Section 2 Length = %d \n",s2_len); 
      printf(" Section 3 Length = %d \n",s3_len); 
      printf(" Section 4 Length = %d \n",s4_len); 
      printf(" Section 5 Length = %d \n",s5l); 
      goto ABORT;
    }

    BM->Section0.message_length = IntToInt3( n );

    BM->Section1.length = IntToInt3( s1_len );
    BM->Section2.length = IntToInt3( s2_len );
    BM->Section3.length = IntToInt3( s3_len );
    BM->Section4.length = IntToInt3( s4_len );

    /*  addted code to change integer to integer structure to solve endian */
    /* problem */
    BM->Section3.data_subsets = IntToInt2(BM->subset_index);

/* 050798 remove LAH
#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 7 )
        BUFR_Print( 0, 0, BUFR_Cntl.bufr_log );
#endif
*/

    /* Write each section */

    /* 100997 LAH: Added int & size_t cast */
    if( (n= (int) fwrite( s0p, 1, (size_t) s0l, fp )) != s0l )
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 0" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 0");
        (void) fclose( fp );
        return 1;
    }

    /* 100997 LAH: Added int & size_t cast */
    if( (n= (int) fwrite( s1p, 1, (size_t) s1l, fp )) != s1l )
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 1");
        (void) fclose( fp );
        return 1;
    } else if( (n= (int) fwrite( s1dp, 1, (size_t) s1dl, fp )) != s1dl )
     /* 100997 LAH: Added int & size_t cast */
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 1 bit stream" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 1 bit stream");
        (void) fclose( fp );
        return 1;
    }

    if( any_s2_data )
    {
        /* 100997 LAH: Added int & size_t cast */
        if( (n= (int) fwrite( s2p, 1, (size_t) s2l, fp )) != s2l )
        {
            BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 2" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
                "Can't write Section 2");
            (void) fclose( fp );
            return 1;
        }
        /* 100997 LAH: Added int & size_t cast */
        else if( (n= (int) fwrite( s2dp, 1, (size_t) s2dl, fp )) != s2dl )
        {
            BUFR_Err_Set("BUFR_Write_Msg","Can't write Section 2 bit stream");
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
                "Can't write Section 2 bit stream");
            (void) fclose( fp );
            return 1;
        }
    }

    /* 100997 LAH: Added int & size_t cast */
    if( (n= (int) fwrite( s3p, 1, (size_t) s3l, fp )) != s3l )
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 3" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 3");
        (void) fclose( fp );
        return 1;
    } else if( (n= (int) fwrite( s3dp, 1, (size_t) s3dl, fp )) != s3dl )
    /* 100997 LAH: Added int & size_t cast */
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 3 bit stream" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 3 bit stream");
        (void) fclose( fp );
        return 1;
    }

    /* 100997 LAH: Added int & size_t cast */
    if( (n= (int) fwrite( s4p, 1, (size_t) s4l, fp )) != s4l )
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 4" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 4");
        (void) fclose( fp );
        return 1;
    } else if( (n= (int) fwrite( s4dp, 1, (size_t) s4dl, fp )) != s4dl )
    /* 100997 LAH: Added int & size_t cast */
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 4 bit stream" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 4 bit stream");
        (void) fclose( fp );
        return 1;
    }

    /* 100997 LAH: Added int &size_t cast */
    if( (n= (int) fwrite( s5p, 1, (size_t) s5l, fp )) !=  s5l )
    {
        BUFR_Err_Set( "BUFR_Write_Msg", "Can't write Section 5" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Write_Msg", 
            "Can't write Section 5");
        (void) fclose( fp );
        return 1;
    }
/*
  fseek(fp, 0, SEEK_END);

  size = (int) ftell(fp);
  printf(" size = %d\n",size);

    (void) fclose( fp );

    fp = fopen( BM->FileName, "r" );
  
  fseek(fp, 0, SEEK_END);

  size = (int) ftell(fp);
  printf(" size = %d\n",size);
*/
    (void) fclose(fp);
    return 0;

ABORT:

    BUFR_Err_Log( "BUFR_Write_Msg" );
    (void) fclose( fp );
    return 1;
}
