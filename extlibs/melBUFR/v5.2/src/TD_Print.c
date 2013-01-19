/*
 * TableD_Print - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern TableD_t* TableD;

#if PROTOTYPE_NEEDED

void TableD_Print( FILE* fp )

#else

void TableD_Print( fp )
FILE*       fp;

#endif
{

    TableD_Sequence_t* DSeq;
    TableD_Entry_t*    DEnt;
    uint_t             f, x, y;

    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "| Table D |\n" );
    fprintf( fp, "+---------+\n" );
    fprintf( fp, "\n" );

    for( DSeq=TableD->head->next; DSeq!= TableD->tail; DSeq=DSeq->next )
    {
        DEnt = DSeq->head;

        FXY_Unpack( DEnt->fxy_value, &f, &x, &y );

        fprintf( fp, "%d-%02d-%03d: ", f, x, y );

        for( DEnt=DEnt->next; DEnt != DSeq->tail; DEnt=DEnt->next )
        {
            FXY_Unpack( DEnt->fxy_value, &f, &x, &y );
            fprintf( fp, "%d-%02d-%03d ", f, x, y );
        }

        fprintf( fp, "\n" );
    }

    fprintf( fp, "\n" );
    fflush( fp );
}
