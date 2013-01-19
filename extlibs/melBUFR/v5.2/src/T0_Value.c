/*
 * Table0_Value - VERSION: %I%  %E% %T%
 */
/*
 * Table0_Value(): Given a table index value, return the corresponding center
 * name.  Return center with name set to NULL for invalid indices or reserved
 * centers.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

Table0_t Table0_Value( int Table0_index )

#else

Table0_t Table0_Value( Table0_index )
int Table0_index;

#endif
{
    extern Table0_t Table0[MAX_TABLE_0_ENTRIES];

    Table0_t center;

    memset( (char*) &center, 0, sizeof(Table0_t) );

    if( Table0_index < 0 || Table0_index >= MAX_TABLE_0_ENTRIES )
    {
        center.name = NULL;
        return center;
    }

    /* Check for "reserved" string. */

    center = Table0[ Table0_index ];

    if( center.name == NULL )
    {
        center.name = NULL;
        return center;
    }

    if( strncmp( center.name, "RESERVED", strlen( "RESERVED" ) ) == 0 )
    {
        center.name = NULL;
        return center;
    }

    return center;
}
