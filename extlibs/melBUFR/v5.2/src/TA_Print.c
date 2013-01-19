/*
 * TableA_Print - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void TableA_Print( FILE* fp )

#else

void TableA_Print( fp )
FILE*       fp;

#endif
{
    extern TableA_t TableA[MAX_TABLE_A_ENTRIES];

    int       i;
    TableA_t* tp;

    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "| Table A |\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "\n" );

    for( i=0, tp=&TableA[0]; i < MAX_TABLE_A_ENTRIES; i++, tp++ )
        fprintf( fp, "%3d %s\n", i, *tp );

    fprintf( fp, "\n" );
    fflush( fp );
}
