/*
 * BUFR_FindSequence - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_FindSequence - Find a Table D (3-XX-YYY) descriptor which
 * matches the given array of descriptors.  Return 1 if a match was
 * found, otherwise return 0 to indicate that a match was not found.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_FindSequence( FXY_t* FXY_Vals, int NumVals, FXY_t* ReturnVal )

#else

int BUFR_FindSequence( FXY_Vals, NumVals, ReturnVal )
FXY_t* FXY_Vals;
int    NumVals;
FXY_t* ReturnVal;

#endif
{
    extern TableD_t* TableD;
    extern BUFR_Cntl_t BUFR_Cntl;

    TableD_Sequence_t* Dseq;
    TableD_Entry_t*    Dent;
    FXY_t*             fxy;
    int                i;

    if( FXY_Vals == NULL )
    {
        BUFR_Err_Set( "BUFR_FindSequence", "NULL array of FXY values" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_FindSequence", 
          "NULL array of FXY values" );
        return 0;
    }

    if( NumVals < 2 )
    {
        BUFR_Err_Set( "BUFR_FindSequence", "Number of FXY values < 2" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_FindSequence", 
          "Number of FXY values < 2" );
        return 0;
    }

    for( Dseq=TableD->head->next; Dseq != TableD->tail; Dseq=Dseq->next )
    {
        if( Dseq->num_entries != NumVals )
            continue;

        Dent = Dseq->head->next;
        fxy  = FXY_Vals;

        for( i=0; Dent!=Dseq->tail && i<NumVals; i++, fxy++, Dent=Dent->next )
        {
            if( Dent->fxy_value != *fxy )
                goto NO_MATCH;
        }

        *ReturnVal = Dseq->head->fxy_value;
        return 1;

NO_MATCH:
        continue;
    }

    return 0;
}
