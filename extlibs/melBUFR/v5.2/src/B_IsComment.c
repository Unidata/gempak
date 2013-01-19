/*
 * BUFR_IsComment - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_IsComment - Return 1 if the given line is a BUFR table comment line,
 * otherwise 0.  A comment line either begins with '#' or consists entirely
 * of whitespace.  (including returns, form feeds, and vertical tabs).
 */
/*
 * CHANGE LOG 
 *
 * 092997  LAH: Added cast to correct Linux warning 
 * 101097  LAH: Added ulong_t cast
 */

#include <mel_bufr.h>
#include <ctype.h>      /* for isspace() */

#if PROTOTYPE_NEEDED

int BUFR_IsComment( char* Line )

#else

int BUFR_IsComment( Line )
char* Line;

#endif
{
    if( *Line == '#' )
        return 1;

    /* 092997  LAH: Added cast to correct Linux warning */
    for( ; *Line != (char) NULL; Line++ )
    {
        /* 101097 LAH: Added ulong_t cast */
        if( isspace( (ulong_t) *Line ) )
            continue;
        else
            return 0;
    }

    return 1;
}
