/*
 * DataList_Destroy - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;     /* JRA021397 */

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         DataList_Destroy
 
  DESCRIPTION:         Destroy the data list
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	void DataList_Destroy( DataList_t* DL )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
	DL	     DataList_t*    In	     Pointer to the data list
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  free		Frees local memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg	BUFR_Msg_t	External BUFR message structure
  de		DataEntry_t	Pointer to a data entry structure
  next_DE	DataEntry_t	Pointer to a data entry structure
  encval	EncVal_t	Pointer ta a encoded value structure
  n		int		loop counter
 
  METHOD:
	If the data list has no items  return
	Loop on the data list items
	  Perform free to deallocate memory for the fxy list
	  Loop on number of encoded values
	    Perform free to deallocate memory for the encoded values
	  End loop
	  Perform free to deallocate the list item
	End loop
	Perform free to deallocate the head of the list
	Perform free to deallocate the tail of the list
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
* ************************************************************************
*
*      MODIFICATION LOG
*
**************************************************************************
*
*     3/05/1996
*     BY LOUIS HEMBREE
*     Added code to free memory allocation for data list head and tail
*
*     3/07/1996
*     BY LOUIS HEMBREE
*     Added code to deallocate memory allocated in HexStr_Set for
*     EncVal_t structure member of DataEntry_t structure
*
***************************************************************************
    INITIAL INSTALLATION:
 
..............END PROLOGUE...................................... */

#if PROTOTYPE_NEEDED

void DataList_Destroy( DataList_t* DL )

#else

void DataList_Destroy( DL )
DataList_t* DL;

#endif
{

    DataEntry_t *de, *next_DE;
    EncVal_t    *encval;        /* added 3/8/96 for deallocation of hex str */
    int     n;                  /* loop counter */

    if( DL == NULL )
        return;

    de = DL->head->next;

    while( de != DL->tail )
    {
      next_DE = de->next;

      if( de->fxy_list != NULL )
          free( (void*) de->fxy_list );

      /*  Deallocate memory allocated by HexStr_Set for     */
      /*  de->value->value             (+ 3/07/96)          */
      /*  loop on num_values is for arrays                  */
      encval = de->value;
      if( de->num_values > 0 )
      {
        for ( n = 0; n < de->num_values; n++)
        {
          if( encval->value != NULL )
              free( (void*) encval->value );
          encval++;
        }
      }

      if( de->value != NULL )
          free( (void*) de->value );

      free( (void*) de );

      de = next_DE;
    }

    DL->head->next = DL->tail;

    /* free allocation for head and tail   (+3/5/96) */

    free( (void*) DL->head );
    free( (void*) DL->tail );

    BUFR_Msg.last_de = (DataEntry_t*) NULL;
}
