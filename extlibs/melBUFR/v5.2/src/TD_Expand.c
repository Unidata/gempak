/*
 * TableD_Expand - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
 * Convert the given Table D entry (containing a Table D FXY value)
 * into a list of Table B descriptors.  If another Table D entry is
 * encountered, expand it as well.
 *
 * Return 1 on error, else 0.
 *
 * NOTE: This function should only be called by TableD_Read().
 */

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         TableD_Expand
 
  DESCRIPTION:         Convert the given Table D entry (containing a 
			Table D FXY value) into a list of Table B 
			descriptors.  If another Table D entry is
			encountered, expand it as well.  Return 1 on 
			error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
		int TableD_Expand( TableD_Entry_t* ExpEnt )	
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  ExpEnt	     TableD_Entry_t input	Table D entry
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Entry is not Table D	Write error message
			Return with error
  Table D descriptor  	Write error message
  references itself	Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  TableD_Match		Checks an entry against the Table D
  FXT_IsTableD		Checks an FXY against the Table D
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  ExpSeq	TableD_Sequence_t	Table D Sequence
  NewEnt	TableD_Entry_t	New Table D entry
  ExpEntNext	TableD_Entry_t	Next Table D entry
  e1		TableD_Entry_t	Local ExpEnt
  e2		TableD_Entry_t	Local next ExpSeq
  buf		char		Character string for error messages
 
  METHOD:
	Perform TableD_Match to get Table D sequence
	If error occurs then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    022498 LAH:  Added prints to bufr_log file.
  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int TableD_Expand( TableD_Entry_t* ExpEnt )

#else

int TableD_Expand( ExpEnt )
TableD_Entry_t*    ExpEnt;

#endif
{
    TableD_Sequence_t* ExpSeq;
    TableD_Entry_t*    NewEnt;

    TableD_Entry_t     *ExpEntNext, *e1, *e2;

    char buf[128];

    /* This shouldn't fail but check it anyway. */

    if( (ExpSeq = TableD_Match( ExpEnt->fxy_value )) == NULL )
    {
        sprintf( buf, "Can't find Table D sequence for %s",
            FXY_String( ExpEnt->fxy_value ) );
        BUFR_Err_Set( "TableD_Expand", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "TableD_Expand", buf);
         return 1;
    }

    /*
     * Replace the current FXY value of 3-XX-YYY with the first descriptor
     * in a list of descriptors and insert copies of the remaining
     * descriptors between ExpEnt and ExpEnt->next.
     */

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 8 )
    {
        fprintf(BUFR_Cntl.bufr_log,
	    "TableD_Expand(%s)\n", FXY_String( ExpEnt->fxy_value ) );
    }
#endif

    ExpEntNext = ExpEnt->next;

    e1 = ExpEnt;
    e2 = ExpSeq->head->next;

    while( e2 != ExpSeq->tail )
    {
        e1->fxy_value = e2->fxy_value;
        e1->item      = e2->item;

        if( FXY_IsTableD( e1->fxy_value ) )
        {
            /*
             * While expanding a Table D descriptor we've encountered another
             * Table D descriptor.  Make sure that the descriptor we're about
             * to expand isn't the same sequence we're already expanding.
             */

            if( e1->fxy_value == ExpSeq->head->fxy_value )
            {
                sprintf( buf,
                    "Table D descriptor %s indirectly references itself",
                    FXY_String( e1->fxy_value ) );
                BUFR_Err_Set( "TableD_Expand", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
		     "TableD_Expand", buf);
                return 1;
            }

            if( TableD_Expand( e1 ) )
                return 1;
            else
                continue;
        }

        if( e2->next == ExpSeq->tail )
            break;

        NewEnt = (TableD_Entry_t*) malloc( sizeof(TableD_Entry_t) );

        if( NewEnt == NULL )
            return 1;

        NewEnt->next = ExpEntNext;
        e1->next     = NewEnt;

        e1 = e1->next;
        e2 = e2->next;
    }

    return 0;
}
