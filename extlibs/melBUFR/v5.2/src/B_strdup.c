/*
 * BUFR_strdup - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
 * CHANGE LOG 
 *
 * 100897 LAH: Added int and uint_t casts 
 *
 */

#if PROTOTYPE_NEEDED

char* BUFR_strdup( char* s1 )

#else

char* BUFR_strdup( s1 )
char* s1;

#endif
{
    int i, len;
    char* s2;

    if( s1 == NULL )
        return NULL;

    /* 100897 LAH: Added int cast */
    len = (int) strlen( s1 );
    len++;                  /* Count NULL-byte terminator */

    /* 100897 LAH: Added uint_t cast */
    s2 = (char*) malloc( (uint_t) len );

    for( i=0; i < len; i++ )
        s2[i] = s1[i];

    return s2;
}
