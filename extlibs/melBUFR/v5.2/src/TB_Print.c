/*
 * TableB_Print - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void TableB_Print( FILE* fp )

#else

void TableB_Print( fp )
FILE*       fp;

#endif
{
    extern TableB_t* TableB;

    TableB_Entry_t* BE;
    Descriptor_t*   d;
    uint_t          f, x, y;

    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "| Table B |\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "\n" );
    fprintf( fp, "F-XX-YYY %-16s %-10s Description\n", "Units", "UnitType" );

    for( f=0; f < 80; f++ )
        fprintf( fp, "-" );

    fprintf( fp, "\n" );

    for( BE=TableB->head->next; BE != TableB->tail; BE=BE->next )
    {
        d = BE->item;

        FXY_Unpack( d->fxy_value, &f, &x, &y );

        fprintf( fp, "%d-%02d-%03d ", f, x, y );

        fprintf( fp, "%-16s ", d->units );

        switch( d->units_type )
        {
            case UNKNOWN_UNIT: fprintf( fp, "%-10s ", "UNKNOWN"    ); break;
            case CCITT_IA5:    fprintf( fp, "%-10s ", "CHARACTER"  ); break;
            case CODE_TABLE:   fprintf( fp, "%-10s ", "CODE_TABLE" ); break;
            case FLAG_TABLE:   fprintf( fp, "%-10s ", "FLAG_TABLE" ); break;
            case NUMERIC:      fprintf( fp, "%-10s ", "NUMERIC"    ); break;

            default:
                fprintf( fp, "%-10s", "???" );
        }

        fprintf( fp, "%s\n", d->description );
    }

    fprintf( fp, "\n" );
    fflush( fp );
}
