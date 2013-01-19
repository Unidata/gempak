/*
 * TableD_Sequence_Init - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Init - Create a new descriptor sequence and insert it in Table D.
 */

#include <mel_bufr.h>
extern TableD_t* TableD;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         TableD_Sequence_Init
 
  DESCRIPTION:         Create a new descriptor sequence and insert it in 
			Table D
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	TableD_Sequence_t* TableD_Sequence_Init( FXY_t FXYval )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FXYval		FXY_t	    Input	FXY descriptor
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Bad memory allocation	Free memory allocated
			Return
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  malloc		Allocates memory to structures and arrays
  free		Deallocates allocated memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  TableD	TableD_t	External Table D
  NewDS		TableD_Sequence_t	Pointer to new Table D sequence
  LastDS	TableD_Sequence_t	Pointer to last Table D sequence
  ThisDS	TableD_Sequence_t	Pointer to this Table D sequence
 
  METHOD:
	Perform malloc to allocate memory for the sequence structure
	If there is an error in allocation return
	Perform malloc to allocate memory for sequence head entry
	If there is an error in allocation then
	  Free the allocated memory
	  return
	Endif
	Perform malloc to allocate memory for sequence tail entry
	If there is an error in allocation then
	  Free the allocated head memory
	  Free the allocated tail memory
	  return
	Endif
	Fill new structure entry elements with data
	Loop to find tail of structure
	End loop
	Insert new entry between last sequence and the tail
	Return the new structure
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

TableD_Sequence_t* TableD_Sequence_Init( FXY_t FXYval )

#else

TableD_Sequence_t* TableD_Sequence_Init( FXYval )
FXY_t FXYval;

#endif
{

    TableD_Sequence_t *NewDS, *LastDS, *ThisDS;

    NewDS =(TableD_Sequence_t*) malloc( sizeof(TableD_Sequence_t) );

    if( NewDS == NULL )
        return NULL;

    NewDS->head = (TableD_Entry_t*) malloc( sizeof(TableD_Entry_t) );

    if( NewDS->head == NULL )
    {
        free( (void*) NewDS );
        return NULL;
    }

    NewDS->tail = (TableD_Entry_t*) malloc( sizeof(TableD_Entry_t) );

    if( NewDS->tail == NULL )
    {
        free( (void*) NewDS->head );
        free( (void*) NewDS );
        return NULL;
    }

    /* Store Table D descriptor (3-XX-YYY) in the head and tail. */

    NewDS->head->fxy_value = FXYval;
    NewDS->tail->fxy_value = FXYval;

    NewDS->head->item = (Descriptor_t*) NULL;
    NewDS->head->next = NewDS->tail;

    NewDS->tail->item = (Descriptor_t*) NULL;
    NewDS->tail->next = (TableD_Entry_t*) NULL;

    /*
     * Sequence is initialized.  Add it after the final entry in Table D.
     */

     LastDS = TableD->head;
     ThisDS = TableD->head->next;

     while( ThisDS != TableD->tail )    /* Find tail */
     {
        LastDS = ThisDS;
        ThisDS = ThisDS->next;
     }

     /* Insert new entry between last sequence and Table D tail. */

     LastDS->next = NewDS;
     NewDS->next  = TableD->tail;

     NewDS->num_entries = 0;

    return NewDS;
}
