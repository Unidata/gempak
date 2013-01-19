/*
 * EncVal_Print - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void EncVal_Print( EncVal_t EV, FILE*  fp )

#else

void EncVal_Print( EV, fp )
EncVal_t EV;
FILE*  fp;

#endif
{
    int      i;
    HexStr_t hs;

    if( fp == NULL )
        fp = stdout;

    if( EncVal_IsBad(EV) )
    {
        fprintf( fp, "Bad EncVal_t\n" );
        return;
    }

    for( i=0, hs=EV.value; i < EV.nbits; i+=BITS_IN_BYTE, hs++ )
        fprintf( fp, "%02X ", *hs );

    fprintf( fp, "\n" );

    fflush( fp );
}
