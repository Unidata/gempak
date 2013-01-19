/*
 * DataList_Put - VERSION: %I%  %E% %T%
 */
/*
 * DataList_Put() - Add a descriptor to a linked-list of values.
 * Return 1 on error, else 0.
 * 
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;     /* JRA021397 */
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         DataList_Put
 
  DESCRIPTION:         Add a descriptor to a linked-list of values.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int DataList_Put( DataList_t* DL, FXY_t* FXY_List, int NumFXYs,
                  EncVal_t* Value, int NumValues )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
	DL	     DataList_t*    Out		Pointer to data list
    FXY_List	     FXY_t	    In		Array of FXYs
     NumFXYs	     Int	    In		FXY_List size
      Value	     EncVal_t*	    In		Address of element added
    NumValues	     Int	    In		Number of values
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  No Data List Sent	Write Error message
			Return with error
  No FXY List Sent	Write Error message
			Return with error
  Number of FXYs < 0	Write Error message
			Return with error
  Number of Values < 0	Write Error message
			Return with error
  Bad malloc 		Write Error message
			Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Write error messages to a buffer
  malloc		Get more memory for an array
  free		Frees memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  newDe         DataEntry_t     Pointer to a data entry structure
  lastDE        DataEntry_t     Pointer to a data entry structure
  fin           FXY_t           FXY list
  fout          FXY_t           FXY list
  num_bytes	int		Number of bytes in an entry
  i		int		Loop variable
 
  METHOD:
	If there is no data list then
          Write error message
          Return with an error
        Endif
	If there is no FXY list then
          Write error message
          Return with an error
        Endif
	If the Number of FXYs is less than one then
          Write error message
          Return with an error
        Endif
	If the Number of values is less than zero then
          Write error message
          Return with an error
        Endif
	If there is an error while requesting data list memory then
          Write error message
          Return with an error
        Endif
	If there is an error while requesting FXY list memory then
          Write error message
          return with an error
        Else
	  Loop on Number of FXYs
	    If the input FXY equals FXY_IGNORE then
		Remove the count 
	    Else
		Add the input FXY to the output list
	    Endif
	  EndLoop
	Endif

	If the Number of values is greater than zero  the
	  If there is an error while requesting memory for enclosed value then
	    Write error message
	    Perform free to free memory
            return with an error
	  Else
	    

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    100897 LAH: Added int and uint_t casts 
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int DataList_Put( DataList_t* DL, FXY_t* FXY_List, int NumFXYs,
                  EncVal_t* Value, int NumValues )

#else

int DataList_Put( DL, FXY_List, NumFXYs, Value, NumValues )
DataList_t* DL;
FXY_t*      FXY_List;       /* Array of FXYs (for sequences) */
int         NumFXYs;        /* FXY_List size (for sequences) */
EncVal_t*   Value;          /* Address of element to be added */
int         NumValues;      /* Number of values */

#endif
{

    DataEntry_t* newDE;
    DataEntry_t* lastDE;

    int    num_bytes;
    int    i;
    FXY_t *fin, *fout;

    if( DL == NULL )
    {
        BUFR_Err_Set( "DataList_Put", "NULL DataList_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
            "NULL DataList_t pointer" );
        return 1;
    }

    if( FXY_List == NULL )
    {
        BUFR_Err_Set( "DataList_Put", "NULL FXY_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
            "NULL FXY_t pointer" );
        return 1;
    }

    if( NumFXYs < 1 )
    {
        BUFR_Err_Set( "DataList_Put", "Number of FXY values < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
            "Number of FXY values < 1" );
        return 1;
    }

    if( NumValues < 0 )
    {
        BUFR_Err_Set( "DataList_Put", "Number of encoded values < 0" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
            "Number of encoded values < 0" );
        return 1;
    }

    /* Create new data entry */

    if( (newDE = (DataEntry_t*) malloc( sizeof(DataEntry_t) )) == NULL )
    {
        BUFR_Err_Set( "DataList_Put", "Can't allocate new entry" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
            "Can't allocate new entry" );
        return 1;
    }
    else
        (void) memset( (char*) newDE, 0, sizeof(DataEntry_t) );

    /* Copy list of FXY values to new DataEntry_t (newDE) */

    /* 100897 LAH: Added int cast */
    num_bytes = NumFXYs * (int) sizeof( FXY_t );

    /* 100897 LAH: Added uint_t cast */
    if( (newDE->fxy_list = (FXY_t*)malloc( (uint_t) num_bytes)) == NULL )
    {
        BUFR_Err_Set( "DataList_Put", "Can't FXY list" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
            "Can't FXY list" );
         free( (void*) newDE );
        return 1;
    }
    else
    {
        fin  = FXY_List;
        fout = newDE->fxy_list;

        newDE->num_fxys = NumFXYs;

        for( i=0; i < NumFXYs; i++, fin++ )
        {
            if( *fin == (FXY_t)FXY_IGNORE )
                newDE->num_fxys--;      /* Remove count for FXY_IGNORE */
            else
                *(fout++) = *fin;
        }
    }

    /* Copy (possible) encoded values to new DataEntry_t */

    if( NumValues > 0 )
    {
        /* 100897 LAH: Added int cast */
        num_bytes = NumValues * (int) sizeof( EncVal_t );

            /* 100897 LAH: Added uint_t cast */
        if( (newDE->value = (EncVal_t*)malloc( (uint_t) num_bytes)) == NULL )
        {
            BUFR_Err_Set( "DataList_Put", "Can't copy value(s)" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Put", 
	         "Can't copy value(s)" );
            free( (void*) newDE );
            return 1;
        } else
        {
            /* 100897 LAH: Added uint_t cast */
            memcpy( (char*) newDE->value, (char*) Value, (uint_t)num_bytes );
            newDE->num_values = NumValues;
        }
    } else
    {
        newDE->value      = NULL;
        newDE->num_values = NumValues;
    }

    /* Everything went OK.  Insert new entry before tail. */

    /*
    * JRA021397 - This call to DataList_Last() is killing performance.
    *
    lastDE = DataList_Last( DL );
    *
    * Using the last_de member of BUFR_Msg instead.
    */

    lastDE = BUFR_Msg.last_de;

    newDE->next  = lastDE->next;
    lastDE->next = newDE;

    BUFR_Msg.last_de = newDE;

    return 0;
}
