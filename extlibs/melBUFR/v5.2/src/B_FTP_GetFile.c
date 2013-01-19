/*
 * BUFR_FTP_GetFile - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_FTP_GetFile - Get a file (i.e., a BUFR Table) via anonymous FTP.
 * Return 0 on success, 1 on error.
 *
 * Note that only "FileName" is required.  If FTP_Host or FTP_Path are set
 * to NULL, the appropriate environment variable is used.  If the appropriate
 * environment variable is missing, then a default value is used.
 *
 * Normally, the user does not need to set any of the environment variables
 * searched for -- the default values are good enough.  Environment variables
 * allow for non-default FTP transfers without having to recompile any code.
 */
/*
 * LAST MODIFICATION 
 *
 * 092997  LAH: Added char casts to correct Linux warnings 
 * 102097  LAH: Added int cast 
 * 102097 LAH: Added include sys/stat.h  for chmod 
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>
/* 102097 LAH: Added include sys/stat.h  for chmod */
#include <sys/stat.h>
#ifdef _WIN32
#include <io.h>
#endif

#if PROTOTYPE_NEEDED

int BUFR_FTP_GetFile( char* FullFileName, char* FTP_Host, char* FTP_Path )

#else

int BUFR_FTP_GetFile( FullFileName, FTP_Host, FTP_Path )
char* FullFileName; /* Path and name of the file to retrieve.        */
char* FTP_Host;     /* Name of the FTP host to connect to.           */
char* FTP_Path;     /* Location of the FTP file at FTP_Host.         */

#endif
{
    static char* Default_BUFR_FTP_Host   =  "ftp.nrlmry.navy.mil";
    static char* Default_BUFR_FTP_Path   =  "/pub/receive/BUFR_TABLES";
    extern BUFR_Cntl_t BUFR_Cntl;

    /* Since this is anonymous FTP, the password doesn't really matter. */

    static char* BUFR_FTP_Password = "bufr_ftp";

    char  local_path[256];  /* Directory portion of FullFileName. */
    char  file_name[256];   /* File name portion of FullFileName. */
    char* cp;
    int   i;

    char  ftp_script[256];  /* File of Bourne shell commands for FTP. */
    FILE* fp;
    int   ftp_stat;

    if( FullFileName == NULL || *FullFileName == (char) NULL )
    {
        BUFR_Err_Set( "BUFR_FTP_GetFile", "NULL file name" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_FTP_GetFile", 
	     "NULL file name" );
        return 1;
    }

    /* 092997  LAH: Added cast to correct Linux warning */
    if( FTP_Host == NULL || *FTP_Host == (char) NULL )
    {
        /* Get (possible) environment variable value. */

        if( (FTP_Host = getenv( "MEL_BUFR_FTP_HOST" )) == NULL )
            FTP_Host = Default_BUFR_FTP_Host;
    }

    /* 092997  LAH: Added cast to correct Linux warning */
    if( FTP_Path == NULL || *FTP_Path == (char) NULL )
    {
        /* Get (possible) environment variable value. */

        if( (FTP_Path = getenv( "MEL_BUFR_FTP_PATH" )) == NULL )
            FTP_Path = Default_BUFR_FTP_Path;
    }

    /* Parse FullFileName for the directory and file name. */

    /* 102097 LAH: Added int cast */
    for( i=(int)strlen(FullFileName), cp=FullFileName+i-1; i > 0; i--, cp-- )
    {
        if( *cp == '/' )
        {
            /* 092997  LAH: Added cast to correct Linux warning */
            *cp = (char) NULL;     /* Split FullFileName into two strings. */
            cp++;           /* Point to file name. */
            break;
        }
    }

    if( i == 0 )
    {
        /* No directory specified in FullFileName. */

        sprintf( local_path, "." );
        sprintf( file_name,  "%s", FullFileName );
    }
    else
    {
        sprintf( local_path, "%s", FullFileName );
        sprintf( file_name, "%s", cp );
        *(--cp) = '/';      /* Replace the directory character. */
    }


    /*
     * Create the FTP script and keep the file in /tmp so that in the event
     * the program crashes, a bunch of useless files won't clutter up the
     * local directory (the /tmp directory is always cleared when a machine
     * reboots).
     */

    sprintf( ftp_script, "/tmp/BUFR_FTP.sh" );

    if( (fp=fopen( ftp_script, "w" )) == NULL )
    {
        BUFR_Err_Set( "BUFR_FTP_GetFile", "Can't create FTP script file" );
        return 1;
    }

    fprintf( fp, "#!/bin/sh\n" );
    fprintf( fp, "exec 1>&-\n" );
    fprintf( fp, "exec 2>&-\n" );
    fprintf( fp, "ftp -in %s << STOP\n", FTP_Host );
    fprintf( fp, "user anonymous %s\n",  BUFR_FTP_Password );
    fprintf( fp, "cd %s\n",              FTP_Path );
    fprintf( fp, "lcd %s\n",             local_path );
    fprintf( fp, "get %s\n",             file_name );
    fprintf( fp, "quit\n" );
    fprintf( fp, "STOP\n" );
    fprintf( fp, "exit\n" );

    (void) fclose( fp );

    /*
     * Give the FTP script file read, write, and execute permission
     * for everyone.
     */

    (void) chmod( ftp_script, 0777 );

    /* Execute the FTP script. */

    ftp_stat = system( ftp_script );

    /* Delete the FTP script file. */

    (void) unlink( ftp_script );

    if( ftp_stat != 0 )
    {
        BUFR_Err_Set( "BUFR_FTP_GetFile", "FTP transfer failed" );
	/* LAH 112000 -- print modified to include table file name*/
        fprintf(BUFR_Cntl.bufr_log,"%s: %s%s\n", "BUFR_FTP_GetFile", 
	     "FTP transfer failed:",file_name  );
        return 1;
    }

    /* See if the desired file was actually retrieved. */

    if( !FileExists( FullFileName ) )
    {
        BUFR_Err_Set( "BUFR_FTP_GetFile", "FTP transfer failed" );
	/* LAH 112000 -- print modified to include table file name*/
        fprintf(BUFR_Cntl.bufr_log,"%s: %s%s\n", "BUFR_FTP_GetFile", 
	     "FTP transfer failed2:", file_name );
        return 1;
    }

    return 0;
}
