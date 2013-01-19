/*
 * HexStr_Destroy - VERSION: %I%  %E% %T%
 */
/*
*  HexStr_Destroy - frees memory allocated in HexStr_Set for Hex
*                   string definition.  Return 1 on error, else 0.
*  Louis Hembree, March 1996
*/

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int HexStr_Destroy( EncVal_t* EncVal, int NumValues )

#else

int HexStr_Destroy( EncVal, NumValues )
EncVal_t* EncVal;
int       NumValues;

#endif
{
    EncVal_t* ev;
    int i;

    if( EncVal == NULL )
    {
        BUFR_Err_Log( "HexStr_Destroy" );
        return 1;
    }

    ev = EncVal;

    for( i=0, ev=EncVal; i< NumValues; i++, ev++ )
    {
        if( ev->value == NULL )
        {
            BUFR_Err_Log( "HexStr_Destroy" );
            return 1;
        }

        free( (void*) ev->value );
    }

    return 0;
}
