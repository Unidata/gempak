/*
 * TableB_GetUnits - VERSION: %I%  %E% %T%
 */
/*
 * TableB_GetUnits - Given a string, return corresponding Units_t value.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

Units_t TableB_GetUnits( char* str )

#else

Units_t TableB_GetUnits( str )
char* str;

#endif
{
    typedef struct
    {
        char*   s;
        Units_t v;
    } UnitsAre_t;

    static UnitsAre_t ua[] =
    {
        { "CCITT_IA5",      CCITT_IA5    },
        { "CCITT",          CCITT_IA5    },
        { "ASCII",          CCITT_IA5    },
        { "CHARACTER",      CCITT_IA5    },
        { "Code_Table",     CODE_TABLE   },
        { "Code",           CODE_TABLE   },
        { "Flag_Table",     FLAG_TABLE   },
        { "Flag",           FLAG_TABLE   },
        { "Numeric",        NUMERIC      },
        { "",               UNKNOWN_UNIT }
    };

    UnitsAre_t *u;

    /* 102197 LAH: Added char cast to correct Linux gcc warning */
    for( u=&ua[0]; *u->s != (char)NULL; u++ )
    {
#ifdef _WIN32
        if (stricmp( u->s, str ) == 0 )
#else
        if( strcasecmp( u->s, str ) == 0 )
#endif
            return u->v;
    }

    return NUMERIC;
}
