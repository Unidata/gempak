/*
 * DataList_Print - VERSION: %I%  %E% %T%
 */
/*
 * DataList_Print() - Print the contents of a DataList_t.
 *
 * This is a debug function.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         DataList_Print
 
  DESCRIPTION:         Print the contents of a DataList_t.  This is a 
			debug function.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
				
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	void DataList_Print( FILE* fp )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
	fp		FILE*	     In	     File pointer
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  FXY_String		Puts the FXY value into string form
  EncVal_Print		Prints the encoded value
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  dl            DataList_t      Pointer to a data list structure
  de            DataEntry_t     Pointer to a data entry structure
  dl_num	int		Data list entry number
  i		int		Loop variable
  fxy       	FXY_t           FXY value
  ev            EncVal_t        Encoded Value
 
  METHOD:
	If there is no file declared  set file pointer to standard out
	If there are no values to print then return
	Print header
	Loop on available data list items
	  Print
	EndLoop
 
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

void DataList_Print( FILE* fp )

#else

void DataList_Print( fp )
FILE*       fp;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;

    DataList_t*  dl;
    DataEntry_t* de;
    int          dl_num;
    int          i;
    FXY_t*       fxy;
    EncVal_t*    ev;

    dl = BUFR_Msg.data_list;

    if( fp == NULL )
        fp = stdout;

    if( dl->head->next == dl->tail )    /* No values to print! */
        return;

    fprintf( fp, "\n" );
    fprintf( fp, "DataList values:\n" );
    fprintf( fp, "\n" );

    for( de=dl->head->next, dl_num=1; de != dl->tail; de=de->next, dl_num++ )
    {
        fprintf( fp, "Entry#\tFXYs=%d\t%s", de->num_fxys,
            FXY_String(*de->fxy_list) );

        for( i=1, fxy=de->fxy_list+i; i < de->num_fxys; i++, fxy++)
            fprintf( fp, ",%s", FXY_String( *fxy ) );

        fprintf( fp, "\n" );

        fprintf( fp, "%6d\tVals=%d\n", dl_num, de->num_values );

        if( de->value != NULL )
        {
            for( i=0, ev=de->value; i < de->num_values; i++, ev++)
            {
                fprintf( fp, "\t" );
                EncVal_Print( *ev, fp );
            }
        }

        fprintf( fp, "\n" );
    }

    fprintf( fp, "\n" );

    fflush( fp );
}
