/*
 * PrintDivider - VERSION: %I%  %E% %T%
 */
/*
 * PrintDivider - Print a line, of the specified length, with the given
 * character.  This function is called by BUFR_Print() and BUFR_Info_Print()
 * to separate blocks of text into logical groupings.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void PrintDivider( char c, int len, FILE* fp )

#else

void PrintDivider( c, len, fp )
char  c;
int len;
FILE* fp;

#endif
{
    int i;

    if( fp == NULL )
        fp = stdout;

    for( i=0; i < len; i++ )
        fprintf( fp, "%c", c );

    fprintf( fp, "\n" );
}
