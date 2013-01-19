/*
 * AF_List_Print - VERSION: %I%  %E% %T%
 */
/*
 * AF_List_Print() - Print the contents of a AF_List_t.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

void AF_List_Print( AF_List_t* AL, FILE* fp )

#else

void AF_List_Print( AL, fp )
AF_List_t* AL;
FILE*       fp;

#endif
{
    AF_Entry_t* AE;
    int         al_num;

    if( AL == NULL )
    {
        BUFR_Err_Set( "AF_List_Print", "NULL AF_List_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "AF_List_Print", 
	     "NULL AF_List_t pointer" );
        return;
    }

    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "\n" );
    fprintf( fp, "AF_List values:\n" );

    for( AE=AL->head->next, al_num=1; AE != AL->tail; AE=AE->next, al_num++ )
    {
        fprintf( fp, "%d #bits=%d significance=%d\n", al_num,
            AE->nbits, AE->sig );
    }

    fprintf( fp, "\n" );

    fflush( fp );
}
