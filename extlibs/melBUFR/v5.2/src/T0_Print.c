/*
 * Table0_Print - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void Table0_Print( FILE* fp )

#else

void Table0_Print( fp )
FILE*       fp;

#endif
{
    extern Table0_t Table0[MAX_TABLE_0_ENTRIES];

    int       i;
    Table0_t* tp;

    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "| Table 0 |\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "\n" );

    for( i=0, tp=&Table0[0]; i < MAX_TABLE_0_ENTRIES; i++, tp++ )
        fprintf( fp, "%3d %s\n", i, tp->name );

    fprintf( fp, "\n" );
    fflush( fp );
}
