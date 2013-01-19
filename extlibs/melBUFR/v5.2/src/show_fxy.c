/*
 * read_tables - VERSION: %I%  %E% %T%
 */
/*
 * show_fxy - A program that prints descriptions of given FXY values.
 * Also useful for showing how a Table D FXY value is expanded.
 *
 * Usage:
 *
 *     show_fxy [fxy_value [fxy_value [...]]]
 *
 * Each FXY value given on the command line will be expanded and FXY
 * information displayed.  If no FXY values are given on the command line,
 * the user is repeatedly prompted for FXY values.
 *
 * FXY values may be in the form F-XX-YYY (such as 0-01-002 or 0-1-2) or as
 * a decimal number (such as 001002 or 1002).
 *
 */

#include <mel_bufr.h>

#define MAX_BUF 16

#if PROTOTYPE_NEEDED

int  read_tables( void );
void indent_line( int );
void display_fxy( FXY_t, int );

int main( int argc, char* argv[] )

#else

int  read_tables();
void indent_line();
void display_fxy();

int main( argc, argv )
int    argc;
char** argv;

#endif
{
    int   PromptForFXY;
    char  buf[MAX_BUF];
    int   f, x, y;
    FXY_t fxy_val;

    /* Read BUFR tables. */

    if( read_tables() )
        return 1;

    /* Set up FXY retrieval for parsing or prompting. */

    if( argc > 1 )
    {
        PromptForFXY = 0;

        argv++;             /* Point to first argument. */
        argc--;
    }
    else
        PromptForFXY = 1;

    while( argc > 0 )
    {
        if( PromptForFXY )  /* Get FXY value from user. */
        {
            fprintf( stderr, "Enter FXY value (enter nothing to quit): " );

            if( gets( buf ) == NULL )   /* CTRL-D pressed (EOF). */
                break;

            if( buf[0] == '\0' )        /* Nothing entered. */
                break;
        }
        else                /* Get FXY value from command line. */
        {
            strcpy( buf, *argv );
            argv++;
            argc--;
        }

        /* Convert contents of buf to an FXY value. */

        if( strstr( buf, "-" ) == NULL )
        {
            /* No dashes appear, assume decimal number. */

            if( sscanf( buf, "%d", &f ) != 1 )
            {
                fprintf( stderr, "%s: Entry not a decimal number\n", buf );
                continue;   /* Get next FXY */
            }
            else
                fxy_val = FXY_Pack_Dec( f );
        }
        else
        {
            /* Get F, X, and Y portions of FXY value from buf. */

            if( sscanf( buf, "%d-%d-%d", &f, &x, &y ) != 3 )
            {
                fprintf( stderr, "%s: Entry not in F-XX-YYY format\n", buf );
                continue;   /* Get next FXY */
            }
            else
                fxy_val = FXY_Pack( f, x, y );
        }

        display_fxy( fxy_val, 0 );
    }

    BUFR_Destroy(1);     /* Release allocated memory. */

    return 0;
}

/*
 * read_tables() - Initialize internal BUFR data structures to force BUFR
 * tables to be read.  Initialize for ENCODING to set up an empty BUFR
 * message structure (decoding would require an existing message).
 */

#if PROTOTYPE_NEEDED
int read_tables( void )
#else
int read_tables()
#endif
{
    BUFR_Info_t bi;

    /* Fill out BUFR_Info_t structure with default values. */

    if( BUFR_Info_Init( &bi ) )
    {
        BUFR_perror( "show_fxy" );
        return 1;
    }

    /*
     * The following BUFR_Info_t members are set by BUFR_Info_Init():
     *
    bi.BUFR_Edition                = DEFAULT_BUFR_EDITION;
    bi.BUFR_MasterTable            = DEFAULT_MASTER_TABLE_NUMBER;
    bi.VersionNumberOfMasterTables = DEFAULT_MASTER_TABLE_VERSION_NUMBER;
    bi.OriginatingCenter           = DEFAULT_ORIGINATING_CENTER;
    bi.GeneratingCenter            = DEFAULT_ORIGINATING_CENTER;
    bi.VersionNumberOfLocalTables  = DEFAULT_LOCAL_TABLE_VERSION_NUMBER;
    bi.MinorLocalVersion           = DEFAULT_MINOR_VERSION_NUMBER;
     *
     * If you need to change any of these members, copy the desired line(s)
     * after these comments (i.e., before the call to BUFR_Init()) and supply
     * an appropriate integer value.  Most likely, you will only want to
     * change the local table version information (VersionNumberOfLocalTables
     * and MinorLocalVersion members).
     */

    /* BUFR_Init() will use "bi" to read master and local BUFR tables. */

    fprintf( stderr, "Reading BUFR tables...\n" );

    if( BUFR_Init( &bi, "/dev/null", ENCODE ) )
    {
        BUFR_perror( "show_fxy" );
        return 1;
    }

    return 0;
}

/* Indent line by 'n' spaces. */

#if PROTOTYPE_NEEDED
void indent_line( int n )
#else
void indent_line( n )
int n;
#endif
{
    for( ; n > 0; n-- )
        printf( " " );
}

/*
 * display_fxy() - Given an FXY value, display its information contained
 * in BUFR Table B.  If given an FXY which applies to Table D, expand
 * it and (recursively) print information about each expanded FXY value.
 */

#if PROTOTYPE_NEEDED
void display_fxy( FXY_t fxy, int indentation )
#else
void display_fxy( fxy, indentation )
FXY_t fxy;
int   indentation;
#endif
{
    Descriptor_t* des;      /* Table B descriptor structure */

    /* Values used for Table D expansion. */

    FXY_t* fxy_list;
    int    num_fxys;
    int    i, x_val, y_val;

    indent_line( indentation );

    if( FXY_IsTableB( fxy ) )
    {
        printf( "%s ", FXY_String( fxy ) );

        if( (des=TableB_Get( fxy )) == NULL )
            printf( "[Unknown Table B descriptor]\n" );
        else if( des->description == NULL || *des->description == '\0' )
            printf( "[No description given in Table B]\n" );
        else
            printf( "%s\n", des->description );

        return;
    }
    else if( FXY_IsTableD( fxy ) )
    {
        printf( "%s (SEQUENCE)\n", FXY_String( fxy ) );

        /*
         * Expand Table D values to an array of FXY values and recursively
         * display each expanded FXY value.
         *
         * NOTE: The values in fxy_list will then be Table B values, Table C
         * values, or delayed replicators.
         */

        num_fxys = FXY_Expand( fxy, &fxy_list );

        for( i=0; i < num_fxys; i++ )
            display_fxy( fxy_list[i], indentation+3 );

        free( (void*) fxy_list );
    }
    else if( FXY_IsReplicator( fxy ) )
    {
        printf( "%s (REPLICATOR)\n", FXY_String( fxy ) );
    }

    else if( FXY_IsTableC( fxy ) )
    {
        printf( "%s (OPERATOR) ", FXY_String( fxy ) );

        x_val = FXY_X_Value( fxy );
        y_val = FXY_Y_Value( fxy );

        switch( x_val )
        {
            case 1:
                if( y_val == 0 )
                    printf( "Cancel change data width\n" );
                else
                    printf( "Change data width by %d bits\n", y_val-128 );
                break;

            case 2:
                if( y_val == 0 )
                    printf( "Cancel change scale\n" );
                else
                    printf( "Change scale to %d\n", 10*(y_val-128) );
                break;

            case 3:
                if( y_val == 0 )
                    printf( "Cancel change reference value\n" );
                if( y_val == 255 )
                    printf( "Conclusion of change reference value\n" );
                else
                    printf( "Change reference value\n" );
                break;

            case 4:
                if( y_val == 0 )
                    printf( "Cancel addition associated field\n" );
                else
                    printf( "Add associated field of %d bits\n", y_val );
                break;

            case 5:
                printf( "Signify character (%d characters)\n", y_val );
                break;

            case 6:
/**************************************************************************
                printf( "Signify data width for immediately following " );
                printf( "local descriptor (%d bits)\n", y_val );
**************************************************************************/
                printf( "Signify DW for local descriptor (%d bits)\n",
                    y_val );
                break;

            default:
                printf( "Unknown data description operator\n" );
        }
    }
    else
    {
        printf( "%s [INVALID DESCRIPTOR]\n", FXY_String( fxy ) );
    }
}
