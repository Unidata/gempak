/*
 * BUFR_Read_Tables - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Read_Tables - Read the master and (possible) local tables.  Return 1
 * on error, otherwise 0.
 *
 * If tables file names are NULL, the proper table file names will be
 * determined.
 *
 * If a table file name is given, initially assume that it contains the full
 * path (directory plus filename).  If the file name specified does not exist,
 * search for it in the current directory.  If the file does not exist in the
 * current directory, search for it in the directory specified by the
 * TABLE_DIR_ENV_VARIABLE environment variable (normally MEL_BUFR_TABLES)
 * which is #defined in mel_bufr_defaults.h.
 *
 */
/*
 * BUFR_AtEOD() - Return 1 if the last value in the current dataset has been
 * reached (within the BUFR message currently being decoded) but there are
 * more datasets to decode.
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Added casts to correct Linux warning 
 *
 * 100897 LAH:  100897 LAH: Added int cast
 *
 * 012798  VLP: With the addition of sub-center to originating center, the
 *		file name for local table data will be changed.  If there is
 *		a sub-center it will be added to the file name (see example
 *		below).  If no sub_center, the old form of local file name
 *		will be used.  
 *  022498 LAH:  Added prints to bufr_log file.
 *  100898 VLP: Added Master Table Version Number to the possible value for the
 *		local Tables.  Added Century to be written out.
 */

#include <mel_bufr.h>
#include <stdlib.h>     /* For getenv() */

#if PROTOTYPE_NEEDED

int BUFR_Read_Tables( void )

#else

int BUFR_Read_Tables()

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    char default_table_dir[MAX_PATH_LEN];
    char *table_dir;

    char master_name_fmt[MAX_PATH_LEN];
    char local_name_fmt1[MAX_PATH_LEN];     /* With minor local version */
    char local_name_fmt0[MAX_PATH_LEN];     /* Without minor local version */
/* Both of the next two file names will also have sub-centers in them */
    char local_name_fmt1a[MAX_PATH_LEN];     /* With minor local version */
    char local_name_fmt0a[MAX_PATH_LEN];     /* Without minor local version */
    char local_name_fmtX[MAX_PATH_LEN];	     /* diagnostic print field */

    char local_name_fmt1b[MAX_PATH_LEN];     /* With minor local version */
    char local_name_fmt0b[MAX_PATH_LEN];     /* With minor local version */

    char local_name_fmt1c[MAX_PATH_LEN];     /* Without minor local version */
    char local_name_fmt0c[MAX_PATH_LEN];     /* Without minor local version */
    static char table_letters[5] = { '0', 'A', 'B', 'D', '\0' };

    char *table_char;
    char table_file[MAX_PATH_LEN], *tfp;
    char **bufr_table, *master, *local;

    int      minor_version;
    Table0_t t0v;
    EncVal_t ev;

    char buf[MAX_PATH_LEN];
    int i;

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, ">>> Entering BUFR_Read_Tables\n");
#endif

    /****************************/
    /* Get BUFR Table Filenames */
    /****************************/

    /*
     * Get the default directory.  Environment variable overrides the
     * defined default.
     */

    memset( default_table_dir, 0, MAX_PATH_LEN );

    if( (table_dir=getenv( TABLE_DIR_ENV_VARIABLE )) != NULL )
        strcpy( default_table_dir, table_dir );

    /*
     * Use the BUFR_Info_t structure to construct the default table name
     * patterns.  The table naming convention is as follows:
     *
     * Master Tables
     * -------------
     * B2M-000-003-0
     * B2M-000-003-A
     * B2M-000-003-B
     * B2M-000-003-D
     *  ^   ^   ^  ^
     *  |   |   |  |
     *  |   |   |  +-- BUFR Table
     *  |   |   +----- Master Table Version Number (currently 3)
     *  |   +--------- Master Table                (0=WMO, 10=Oceanography)
     *  +------------- BUFR Edition Number         (currently 2)
     *
     * Local Tables
     * -----------------
     * B2L-058-123-B.255
     * B2L-058-001-D.123
     * B2L-098-123-B  ^
     * B2L-098-001-D  |
     *  ^   ^   ^  ^  +-- Minor Version Number
     *  |   |   |  |
     *  |   |   |  +----- BUFR Table
     *  |   |   +-------- Local Table Version Number
     *  |   +------------ Originating Center          (58=FNOC, 98=ECMWF)
     *  +---------------- BUFR Edition Number         (currently 2)
     *
     * NOTE: Minor version numbers of local tables are only used by
     * NRL, Monterey and FNOC.
     *
     * Local Tables with Sub-Center Ids
     * --------------------------------
     * B2L-058128-123-B.255
     * B2L-058128-001-D.123
     * B2L-098128-123-B  ^
     * B2L-098128-001-D  |
     *  ^  |^||^|  ^  ^  +-- Minor Version Number
     *  |   |  |   |  |
     *  |   |  |   |  +----- BUFR Table
     *  |   |  |   +-------- Local Table Version Number
     *  |   |  +------------ Sub-Center Number
     *  |   +------------ Originating Center          (58=FNOC, 98=ECMWF)
     *  +---------------- BUFR Edition Number         (currently 2)
     *
     *
     * Note:  The Master Table Version Number can be added to the local
     * table file name with adding the sub-center Ids.  
     *
     * Local Tables with Sub-Center Ids And Master Table Version Numbers
     * -----------------------------------------------------------------
     * B2L-058128-03123-B.255
     * B2L-058128-03001-D.123
     * B2L-098128-03123-B  ^
     * B2L-098128-03001-D  |
     *  ^  |^||^| || ^  ^  +-- Minor Version Number
     *  |   |  |   | |  |
     *  |   |  |   | |  +----- BUFR Table
     *  |   |  |   | +-------- Local Table Version Number
     *  |   |  |   +---------- Master Table Version Number
     *  |   |  +------------ Sub-Center Number
     *  |   +------------ Originating Center          (58=FNOC, 98=ECMWF)
     *  +---------------- BUFR Edition Number         (currently 2)
     *
     */

    /* Construct name patterns for default master tables. */

    sprintf( master_name_fmt, "B%dM-%03d-%03d-%%c",
        BUFR_Msg.Info.BUFR_Edition,
        BUFR_Msg.Info.BUFR_MasterTable,
        BUFR_Msg.Info.VersionNumberOfMasterTables );

    /*
     * Construct master table names.  Use table letters '0', 'A', 'B' and 'D'.
     */

    /*  092997  LAH: Added cast to correct Linux warning */
    for( table_char=&table_letters[0]; *table_char != (char)NULL; table_char++ )
    {
        switch( *table_char )
        {
            case '0':
                master     = BUFR_Msg.MasterTable0;
                bufr_table = &BUFR_Msg.MasterTable0;
                break;

            case 'A':
                master     = BUFR_Msg.MasterTableA;
                bufr_table = &BUFR_Msg.MasterTableA;
                break;

            case 'B':
                master     = BUFR_Msg.MasterTableB;
                bufr_table = &BUFR_Msg.MasterTableB;
                break;

            case 'D':
                master     = BUFR_Msg.MasterTableD;
                bufr_table = &BUFR_Msg.MasterTableD;
                break;
        }

        /* Get table file name. */

        /*  092997  LAH: Added cast to correct Linux warning */
        if( master != NULL && *master != (char) NULL )
        {
            tfp = master;
        } else
        {
            sprintf( table_file, master_name_fmt, *table_char );
            tfp = &table_file[0];
        }

        /* Get the table file directory. */

        if( FileExists( tfp ) )
        {
            /* File is in local directory */

            *bufr_table = BUFR_strdup( tfp );
        } else
        {
            /*
             * Look for file in the directory specified by the environment
             * variable TABLE_DIR_ENV_VARIABLE.
             */

            /*  092997  LAH: Added cast to correct Linux warning */
            if( default_table_dir[0] == (char) NULL )
            {
                sprintf( buf, "Master Table %c file \"%s\" %s %s \"%s\" %s",
                    *table_char, tfp,
                    "is not in the local directory\n",
                    "and environment variable", TABLE_DIR_ENV_VARIABLE,
                    "is not set." );

                BUFR_Err_Set( "BUFR_Read_Tables", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Tables:", buf);
                return 1;
            }
            else
                sprintf( buf, "%s/%s", default_table_dir, tfp );

            if( FileExists( buf ) )
            {
                *bufr_table = BUFR_strdup( buf );
            } else
            {
                strcpy( table_file, buf );

                sprintf( buf, "Master Table %c file \"%s\" does not exist",
                    *table_char, table_file );

                BUFR_Err_Set( "BUFR_Read_Tables", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Tables:", buf);
                return 1;
            }
        }
    }

    /********************/
    /* Read BUFR tables */
    /********************/

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        printf( "Reading BUFR Tables\n" );
#endif

    if( Table0_Read( BUFR_Msg.MasterTable0 ) )
    {
        BUFR_Err_Log( "BUFR_Read_Tables" );
        return 1;
    }

    if( TableA_Read( BUFR_Msg.MasterTableA ) )
    {
        BUFR_Err_Log( "BUFR_Read_Tables" );
        return 1;
    }

    if( TableB_Read( BUFR_Msg.MasterTableB ) )
    {
        BUFR_Err_Log( "BUFR_Read_Tables" );
        return 1;
    }

    if( TableD_Read( BUFR_Msg.MasterTableD ) )
    {
        BUFR_Err_Log( "BUFR_Read_Tables" );
        return 1;
    }

    /***************************/
    /* Read local BUFR tables. */
    /***************************/

    /*
     * Get (possible) version of the master tables.
     *
     * Table 0 has been read.  Determine if the originating center uses minor
     * local table version numbers.  If so, store or get the number in or from
     * Section 1, Octet 18, depending on whether encoding or decoding is being
     * performed.
     */

    t0v = Table0_Value( BUFR_Msg.Info.OriginatingCenter );

    if( t0v.name == NULL )
    {
        /*
         * Originating center is unknown or invalid. Print warning message,
         * clear the error, and skip reading local tables.
         */

        fprintf( stderr, "WARNING: Originating center (%d) ",
            BUFR_Msg.Info.OriginatingCenter );
        fprintf( stderr, "is unknown or invalid.  Can't\n" );
        fprintf( stderr, "determine which local table to read\n" );

        /* Don't read local tables. */

        BUFR_Msg.LocalTableB = NULL;
        BUFR_Msg.LocalTableD = NULL;

        return 0;
    }

    /*
     * See if originating center supports minor version numbers.  If t0v.name
     * is NULL, the originating center isn't in Table A so assume that the
     * center does not use minor version numbers.
     */

        EncVal_Init( &ev );

        if( BUFR_ProcType() == TYPE_ENCODE )
        {
            /* Store the Century local version in Section 1, octet 18. */

            if( EncVal_Set( &ev, (double) BUFR_Msg.Info.Century,
                0, 0, BITS_IN_BYTE ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            }

            if( BitStream_Put( &BUFR_Msg.Section1_Data, ev ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            } else
                EncVal_Destroy( ev );

            /* VLP100798:  Generating Center is longer being supported by us */
            if(BUFR_Msg.Info.GeneratingCenter > 0 && 
               BUFR_Msg.Info.GeneratingCenter != GENERATING_CENTER_FLAG)
            {
               fprintf( stderr, "WARNING: Generating Center ID is longer encoded \n");
            }
            /* VLP100798: Store the Software Version # in octet 19. */

            if( EncVal_Set( &ev, (double) BUFR_Msg.Info.SoftVNum,
                0, 0, BITS_IN_BYTE ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            }

            if( BitStream_Put( &BUFR_Msg.Section1_Data, ev ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            } else
                EncVal_Destroy( ev );

            /* VLP100798: Store the Software Version2 # in octet 20. */

            if( EncVal_Set( &ev, (double) BUFR_Msg.Info.SoftV2Num,
                0, 0, BITS_IN_BYTE ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            }

            if( BitStream_Put( &BUFR_Msg.Section1_Data, ev ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            } else
                EncVal_Destroy( ev );

            /* VLP100798: Store the Minor Local Version # in octet 21. */
            if( EncVal_Set( &ev, (double) BUFR_Msg.Info.MinorLocalVersion,
                0, 0, BITS_IN_BYTE ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            }

            if( BitStream_Put( &BUFR_Msg.Section1_Data, ev ) )
            {
                BUFR_Err_Log( "BUFR_Read_Tables" );
                BUFR_Destroy(1);
                EncVal_Destroy( ev );
                return 1;
            } else
                EncVal_Destroy( ev );
        }
        if( !t0v.UsesLocalMinorVersion )
            BUFR_Msg.Info.MinorLocalVersion = 0;

#if FNOC_KLUDGE
        /*
        * JRA021897 - As far as I know, FNOC doesn't use local tables: they lump
        * WMO descriptors -- which are sometimes redefined -- and local
        * descriptors together.
        */

        if( BUFR_Msg.Info.VersionNumberOfLocalTables == 0 &&
            BUFR_Msg.Info.OriginatingCenter != FNOC_ID )
#else
        if( BUFR_Msg.Info.VersionNumberOfLocalTables == 0 )
#endif
        {
          /* Local tables aren't used in this message so don't read them. */

          BUFR_Msg.LocalTableB = NULL;
          BUFR_Msg.LocalTableD = NULL;
          return 0;
        }
 
        /* Construct local table names (Table 0 must be read first). */

        /* Determine minor local version number (if any). */

        t0v = Table0_Value( BUFR_Msg.Info.OriginatingCenter );

        if( t0v.name == NULL )
        {
          BUFR_Err_Log( "BUFR_Read_Tables" );
          return 1;
        }

        if( BUFR_Msg.Info.VersionNumberOfLocalTables > MAX_LOCAL_VERSION_NUMBER )
        {
          sprintf( buf, "%s (%d) %s",
              "Invalid version number of local tables",
              BUFR_Msg.Info.VersionNumberOfLocalTables,
              "in BUFR_Info_t structure" );
          BUFR_Err_Set( "BUFR_Read_Tables", buf );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Tables:", buf);
          return 1;
        }

        /* Construct name patterns for default local tables. */

        sprintf( local_name_fmt0, "B%dL-%03d-%03d-%%c",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

        sprintf( local_name_fmt1, "B%dL-%03d-%03d-%%c.%%03d",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

    /* Construct name patterns for default local tables with sub-centers. */

        sprintf( local_name_fmt0a, "B%dL-%03d%03d-%03d-%%c",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
            BUFR_Msg.Info.SubCenter,
            BUFR_Msg.Info.VersionNumberOfLocalTables );
    
        sprintf( local_name_fmt1a, "B%dL-%03d%03d-%03d-%%c.%%03d",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
          	BUFR_Msg.Info.SubCenter,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

        /* Construct name patterns for default local tables with Master
        *	Table Version Number. */

        sprintf( local_name_fmt0b, "B%dL-%03d-%03d%03d-%%c",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
          	BUFR_Msg.Info.VersionNumberOfMasterTables,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

        sprintf( local_name_fmt1b, "B%dL-%03d-%03d%03d-%%c.%%03d",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
	          BUFR_Msg.Info.VersionNumberOfMasterTables,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

        /* Construct name patterns for default local tables with sub-centers
        *  and Master Table Version Number. */

        sprintf( local_name_fmt0c, "B%dL-%03d%03d-%03d%03d-%%c",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
          	BUFR_Msg.Info.SubCenter, BUFR_Msg.Info.VersionNumberOfMasterTables,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

        sprintf( local_name_fmt1c, "B%dL-%03d%03d-%03d%03d-%%c.%%03d",
            BUFR_Msg.Info.BUFR_Edition, BUFR_Msg.Info.OriginatingCenter,
          	BUFR_Msg.Info.SubCenter, BUFR_Msg.Info.VersionNumberOfMasterTables,
            BUFR_Msg.Info.VersionNumberOfLocalTables );

    if( t0v.UsesLocalMinorVersion )
    {
        /* 100897 LAH: Added int cast */
        minor_version = (int) BUFR_Msg.Info.MinorLocalVersion;

        if( minor_version > MAX_MINOR_VERSION_NUMBER )
        {
            sprintf( buf, "%s (%d) %s",
                "Invalid MinorLocalVersion value", minor_version,
                "in BUFR_Info_t structure" );

            BUFR_Err_Set( "BUFR_Read_Tables", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Tables:", 
	        buf);

            return 1;
        }
    } else
        minor_version = 0;

    /* Construct local table names.  Use only table letters 'B' and 'D'. */

    /*  092997  LAH: Added cast to correct Linux warning */
    for( table_char=&table_letters[2]; *table_char != (char)NULL; table_char++ )
    {
        switch( *table_char )
        {
            case 'B':
                local      = BUFR_Msg.LocalTableB;
                bufr_table = &BUFR_Msg.LocalTableB;
                break;

            case 'D':
                local      = BUFR_Msg.LocalTableD;
                bufr_table = &BUFR_Msg.LocalTableD;
                break;
        }

        for(i = 0; i < 4; i++){
        /* Get table file name. */

        /*  092997  LAH: Added cast to correct Linux warning */
        if( local != NULL && *local != (char) NULL )
        {
          tfp = local;
        } else
        {
          if( minor_version == 0 )
          {
            sprintf( table_file, local_name_fmt0, *table_char );
            if(i == 2) sprintf( table_file, local_name_fmt0a, *table_char );
            if(i == 1) sprintf( table_file, local_name_fmt0b, *table_char );
            if(i == 0) sprintf( table_file, local_name_fmt0c, *table_char );
          } else 
          {
            sprintf(table_file,local_name_fmt1,*table_char,minor_version);
            if(i == 2) sprintf(table_file,local_name_fmt1a,*table_char,	minor_version);
            if(i == 1) sprintf(table_file,local_name_fmt1b,*table_char,	minor_version);
            if(i == 0) sprintf(table_file,local_name_fmt1c,*table_char,	minor_version);
          }

          tfp = &table_file[0];
        }
       
        /* Get the table file directory. */

        if( FileExists( tfp ) )
        {
          /* File is in local directory */
          *bufr_table = BUFR_strdup( tfp );
          break;
        } else
        {
          /*
          * Look for file in the directory specified by the environment
          * variable TABLE_DIR_ENV_VARIABLE.
          */

          /*  092997  LAH: Added cast to correct Linux warning */
          if( default_table_dir[0] == (char)NULL )
          {
            sprintf( buf, "Local Table %c file \"%s\" %s %s \"%s\" %s",
               *table_char, tfp,
               "is not in the local directory\n",
               "and environment variable", TABLE_DIR_ENV_VARIABLE,
               "is not set." );
            if(i == 0)
              continue;

            BUFR_Err_Set( "BUFR_Read_Tables", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Read_Tables:",  buf);
            if(i == 1)
               return 1;
            
          } else
            sprintf( buf, "%s/%s", default_table_dir, tfp );

          /* Unlike the master tables, local tables do not have to exist. */
          if( FileExists( buf ) )
          { 
            *bufr_table = BUFR_strdup( buf );
            break;
          } else if( BUFR_Cntl.Auto_FTP )
          {
            /* JRA021197: Try to FTP the local table. */

#if TRACE_PRINT
            printf( "Retrieving \"%s\" via anonymous FTP...", buf );
#endif
            if( BUFR_FTP_GetFile( buf, NULL, NULL ) )
            { 
#if TRACE_PRINT
              printf( "FTP transfer failed\n" );
#endif
    	        if(i == 0)
                 continue;

              BUFR_Err_Log( "BUFR_Read_Tables" );
                  *bufr_table = NULL;
              if(*table_char == 'B')
                  return 1;
            } else
            { 
#if TRACE_PRINT
              printf( "Got it\n" );
#endif
              *bufr_table = BUFR_strdup( buf );
              break;
            } 
          } else   /*  No file exists */
          {
            sprintf( local_name_fmtX,
                "Local Table %c file \"%s\" %s ",
                *table_char, buf,
                "not found.\n");
/*	    fprintf(BUFR_Cntl.bufr_log," %s\n", local_name_fmtX); */

/*	    if(i == 0) continue; */
/*	    BUFR_Err_Set( "BUFR_Read_Tables", local_name_fmtX ); */
/*	    if(*table_char == 'B') return 1; */
          }
        }
      }
    }

    if( BUFR_Msg.LocalTableB != NULL )
    {
        if( TableB_Read( BUFR_Msg.LocalTableB ) )
            return 1;
    }

    if( BUFR_Msg.LocalTableD != NULL )
    {
        if( TableD_Read( BUFR_Msg.LocalTableD ) )
            return 1;
    }


#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "<<< Exiting BUFR_Read_Tables\n");
#endif

    return 0;
}
