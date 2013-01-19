/*
 * BUFR_ValArray_Destroy - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_ValArray_Destroy( BUFR_Val_t **ValArray,  int num )

#else

void BUFR_ValArray_Destroy(ValArray, num)
BUFR_Val_t **ValArray;
int num;
#endif
{
    BUFR_Val_t *vp;
    int i;
    
    vp = *ValArray;
    for ( i = 0; i< num; i++)
    {
       if ( vp->Val_Type == DT_STRING )
       {
         free(vp->Val.string);
       }
       vp++;
    }
    vp = *ValArray;
    free(vp);
}
