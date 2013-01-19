/*
 * FileExists - VERSION: %I%  %E% %T%
 */
/*
 * FileExists - Return a 1 if a file with the given name exists and 0 if not.
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Added char cast to correct Linux warning 
 */

#include <mel_bufr.h>
#include <sys/types.h>  /* For stat() */
#include <sys/stat.h>   /* For stat() */
#include <errno.h>      /* For system error */

#if PROTOTYPE_NEEDED

int FileExists( char* FileName )

#else

int FileExists( FileName )
char* FileName;

#endif
{
    struct stat buf;

    if( FileName == NULL )      /* NULL pointer */
        return 0;

    /*  092997  LAH: Added cast to correct Linux warning */
    if( *FileName == (char) NULL )     /* File name is "" (empty file name) */
        return 0;

    if( stat( FileName, &buf ) )
    {
        errno = 0;  /* Clear system error */
        return 0;
    }

    /* JRA022497: Only return 1 if a regular file. */

#ifndef _WIN32
    if( S_ISREG( buf.st_mode ) )
      return 1;
    else
      return 0;
#else
    if (buf.st_mode = _S_IFREG)
      return 1;
    else
      return 0;
#endif

}
