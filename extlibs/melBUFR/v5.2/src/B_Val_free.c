/*
 * BUFR_Val_free - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Val_free - Free memory allocated to each BUFR_Val_t 
 *
 * CAUTION: If any of the BUFR_Val_t's have uninitialized or bad pointers, the
 * program may crash.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Val_free( BUFR_Val_t* bv)

#else

void BUFR_Val_free( bv)
BUFR_Val_t* bv;

#endif
{
    int len;

    if( bv== NULL )
        return;

    if ( bv->AF != NULL )
    {
       free( (void*) bv->AF );
    }
    if ( bv->AF_sig != NULL )
    {
       free( (void*) bv->AF_sig );
    }
        bv->num_AFs = 0;

        bv->FXY_Val = (FXY_t) BAD_FXY_VAL;

        if( bv->Val_Type == DT_STRING )
    {
        len=strlen(bv->Val.string);
        memset((char*) bv->Val.string, 0, len); 
        free( (void*) bv->Val.string );
    } else {
            bv->Val.number = 0.0;
    }

    bv->Val_Type = DT_UNKNOWN;
}
