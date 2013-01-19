/*
 * FXY_List_Print - VERSION: %I%  %E% %T%
 */
/* FXY_List_Print - Debug function to print the contents of an FXY_List_t */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void FXY_List_Print( FXY_List_t* FL, FILE* fp )

#else

void FXY_List_Print( FL, fp )
FXY_List_t* FL;
FILE*       fp;

#endif
{
    FXY_Entry_t* FE;
    int n;

    if( FL == NULL )
        return;

    if( fp == NULL )
        fp = stdout;

    if( FL->head->next == FL->tail )
    {
        fprintf( fp, "[Empty FXY List]" );
        fprintf( fp, "\n" );
        fflush( fp );
        return;
    }

    n = 0;

    for( FE=FXY_List_First( FL ); FE != FL->tail; FE=FE->next )
    {
        FXY_Print( FE->fxy, fp );
        n += 8;

        if( FE->next != FL->tail )
        {
            fprintf( fp, ", " );
            n += 2;
        }

        if( n > 72 )
        {
            fprintf( fp, "\n" );
            n = 0;
        }
    }

    fprintf( fp, "\n" );

    fflush( fp );
}
