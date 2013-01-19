/*
 * FXY_List_Expand - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_List_Expand
 
  DESCRIPTION:         Expand sequence (Table D) and replication descriptors 
			in an array of FXY values.  Return NULL on error, 
			otherwise a list of FXY values.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	FXY_List_t* FXY_List_Expand( FXY_t* InList, int NumIn )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
  
  1) It is the calling routine's responsibility to free the memory
  allocated for OutList.
 
  2)During encoding, the expanded list should NOT be placed on a DataList.
 

..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
 
  METHOD:
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    10/09/97 LAH: Added uint_t cast
    12/11/97 LAH: Added code to update new entries in FXY_List_t
                  structure to keep track of the last element and
                  number of elments in the link list.
    02/24/98 LAH:  Added prints to bufr_log file.
    05/20/98 LAH: Modified processing of Table D descriptor.  
                  Sequence inserted but not immediately expanded as
		  previously done.  (NCEP has a Table D descriptor that
		  combines delayed replication descriptor with a 
		  corresponding Table B count descriptor but not the 
		  descriptors to be replicated.  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

FXY_List_t* FXY_List_Expand( FXY_t* InList, int NumIn )

#else

FXY_List_t* FXY_List_Expand( InList, NumIn )
FXY_t* InList;      /* Input list       */
int    NumIn;       /* Length of InList */

#endif
{
    static int R_Calls = -1;    /* For debugging, # recursive calls */

    int i;
    FXY_t* fin;

    FXY_List_t*  OutList;
    register FXY_Entry_t* out_ent;
    FXY_Entry_t* ins_ent;

    TableD_Sequence_t* Dseq;
    TableD_Entry_t*    Dent;

    int    num_fxys;
    FXY_t* fxy_array;
    FXY_t* fxy_ptr;

    FXY_List_t*  expList;
    FXY_Entry_t* exp_ent;

    int X_val, Y_val;

    FXY_Entry_t* tmp_ent;

#if IGNORE_NESTING
    int c201_flag;
    int c202_flag;
#endif

    char errbuf[256];   /* Artbitrarily long error message buffer. */

    if( InList == NULL )
    {
      BUFR_Err_Set( "FXY_List_Expand", "NULL input list pointer" );
      fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Expand", 
          "NULL input list pointer" );
      return NULL;
    }

    if( NumIn < 1 )
    {
      BUFR_Err_Set( "FXY_List_Expand", "Input list size < 1" );
      fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Expand", 
            "Input list size < 1" );
      return NULL;
    }

    /* Create output list */

    if( (OutList=FXY_List_Init()) == NULL )
    {
      BUFR_Err_Log( "FXY_List_Expand" );
      return NULL;
    }

    /* Copy each FXY value in InList to OutList */

    for( i=0, fin=InList; i < NumIn; i++, fin++ )
    {
      if( FXY_List_Put( OutList, *fin ) )
      {
        BUFR_Err_Log( "FXY_List_Expand" );
        (void) FXY_List_Destroy( OutList );
        return NULL;
      }
    }

    R_Calls++;

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 4 )
    {
      for( i=0; i < R_Calls; i++ )
          fprintf(BUFR_Cntl.bufr_log, "\t" );
      fprintf(BUFR_Cntl.bufr_log, "FXY_List_Expand: Expanding " );
      FXY_List_Print( OutList, BUFR_Cntl.bufr_log );
    }
#endif

    /*
    * Scan OutList for sequences and replicated values which need
    * further expansion.
    */

    out_ent = FXY_List_First( OutList );

    while( out_ent != OutList->tail )
    {
      if( FXY_IsTableD( out_ent->fxy ) )
      {
        /* Expand Table D sequence */

        if( (Dseq = TableD_Match( out_ent->fxy )) == NULL )
        {
          sprintf( errbuf, "Descriptor %s is undefined",
                FXY_String( out_ent->fxy ) );
          BUFR_Err_Set( "FXY_List_Expand", errbuf );
          (void) FXY_List_Destroy( OutList );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Expand", 
                    errbuf );
          return NULL;
        }

        /* Insert D_Seq into OutList */
           
        num_fxys = Dseq->num_entries;
        /*
        * Remove the sequence descriptor stored in 'out_ent' from
        * OutList.  Function FXY_List_Remove() returns the entry
        * before the one removed -- save it as the insertion point
        * for entries from expList.
        */

        if( (tmp_ent=FXY_List_Remove( OutList, out_ent )) == NULL )
        {
          BUFR_Err_Log( "FXY_List_Expand" );
          (void) FXY_List_Destroy( expList );
          (void) FXY_List_Destroy( OutList );
          return NULL;
        } else
          out_ent = tmp_ent;

        /* Insert all of the values in 'Dseq' after 'out_ent'. */
        ins_ent = out_ent;
        Dent = Dseq->head->next;

        for ( i = 0; i < num_fxys; i++)
        {
          /* 12/15/97 LAH: Added argument to allow updating */
          /*               of modified FXY_List structure   */
          if( FXY_List_Insert( ins_ent, Dent->fxy_value, OutList ) )
          {
            BUFR_Err_Log( "FXY_List_Expand" );
            /*                    (void) FXY_List_Destroy( Dseq );
            */		    
            (void) FXY_List_Destroy( OutList );
            return NULL;
          }
          Dent = Dent->next;
          ins_ent = ins_ent->next;
        }
        out_ent = out_ent->next;
      } else if( FXY_IsReplicator( out_ent->fxy ) )
      {
        X_val = FXY_X_Value( out_ent->fxy );
        Y_val = FXY_Y_Value( out_ent->fxy );

        if( Y_val == 0 )    /* 1-XX-000, don't expand */
        {
          /*
          * Delayed replication.  The number of replications is unknown
          * so skip over the next 'X_val' number of descriptors.  Don't
          * forget that descriptors with an X value of 31 are not
          * counted as part of the replication.
          *
          * JRA040496 - CORRECTION
          *
          * An FXY value of 0-31-001 or 0-31-002 (delayed and extended
          * delayed replication factor) must immediately follow the
          * 1-XX-000 FXY value.  While these initial delayed
          * replication factors are not counted against the XX value,
          * any other delayed replication descriptors are counted.
          */

#define SHORT_DEL_REP     7936    /* 0-31-000 */
#define DELAYED_REP     7937    /* 0-31-001 */
#define EXT_DELAYED_REP 7938    /* 0-31-002 */

          /* Check for delayed replication factor. */

          out_ent = out_ent->next;

          if( out_ent->fxy != (FXY_t)DELAYED_REP &&
              out_ent->fxy != (FXY_t)SHORT_DEL_REP &&
              out_ent->fxy != (FXY_t)EXT_DELAYED_REP )
          {
            sprintf( errbuf,
                "Replicator 1-%02d-%03d is not followed by %s or %s",
                X_val, Y_val, FXY_String(DELAYED_REP),
                FXY_String(EXT_DELAYED_REP) );
            BUFR_Err_Set( "FXY_List_Expand", errbuf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
		            "FXY_List_Expand", errbuf );
            (void) FXY_List_Destroy( OutList );
            return NULL;
          }

          /*
          * Point to first FXY to be replicated and make sure there
          * are enough descriptors to be replicated.
          */

          out_ent = out_ent->next;

          for( i=0; i < X_val; out_ent=out_ent->next, i++ )
          {
            if( out_ent == OutList->tail )
            {
              sprintf( errbuf, "%s %s 1-%02d-%03d",
                  "Premature end of list while expanding",
                  "replicated sequence", X_val, Y_val );
              BUFR_Err_Set( "FXY_List_Expand", errbuf );
              fprintf(BUFR_Cntl.bufr_log,"%s: %s\n",
  	              "FXY_List_Expand", errbuf );
              (void) FXY_List_Destroy( OutList );
              return NULL;
            }
          }
        } else    /* Expand the next 'X_val' descriptors 'Y_val' times. */
        {
#if DEBUG_PRINT
          if( BUFR_DebugLevel() > 4 )
          {
            for( i=0; i < R_Calls; i++ )
               fprintf(BUFR_Cntl.bufr_log,"\t" );

            fprintf(BUFR_Cntl.bufr_log,
      		        "%s: replicate next %d descriptors %d times\n",
                  FXY_String( out_ent->fxy ), X_val, Y_val );
          }
#endif

          /*
          * Create an expanded FXY list from the next 'X_val' number
          * of entries.
          */

          /* First, create an array of FXY values. */
          num_fxys = X_val;

          /* 100997 LAH: Added uint_t cast */
          fxy_array = (FXY_t*) malloc( (uint_t) num_fxys * sizeof(FXY_t) );

          if( fxy_array == NULL )
          {
            BUFR_Err_Set("FXY_List_Expand", "Can't create FXY array");
            (void) FXY_List_Destroy( OutList );
            return NULL;
          }

          /*
          * Second, initialize fxy_array.  Don't change 'out_ent'
          * because the address is needed for later removal.
          */

          tmp_ent = out_ent->next;

          for( i=0, fxy_ptr=fxy_array; i < num_fxys; i++, fxy_ptr++ )
          {
            *fxy_ptr = tmp_ent->fxy;
            tmp_ent = tmp_ent->next;
          }

          /* Third, expand the array FXY values. */

#if DEBUG_PRINT
          if( BUFR_DebugLevel() > 4 )
          {
            for( i=0; i < R_Calls; i++ )
            fprintf(BUFR_Cntl.bufr_log, "\t" );

            fprintf(BUFR_Cntl.bufr_log,
	              "Recursively expanding replicator...\n" );
          }
#endif

          if( (expList=FXY_List_Expand( fxy_array, num_fxys )) == NULL )
          {
            BUFR_Err_Log( "FXY_List_Expand" );
            free( (void*) fxy_array );
            (void) FXY_List_Destroy( OutList );
            return NULL;
          }

          /* FXY array is no longer needed. */
          free( (void*) fxy_array );

          /*
          * Remove the replication descriptor AND the next 'X_val'
          * number of descriptors stored in 'out_ent' from OutList.
          * Function FXY_List_Remove() returns the entry before the
          * one removed -- save it as the insertion point for entries
          * from expList.
          */

          /* Save address of entry before the current 'out_ent'. */
          ins_ent = FXY_List_Prev( OutList, out_ent );
 
          for( i=-1; i < X_val; i++ )
          {
            if( (tmp_ent=FXY_List_Remove(OutList, out_ent)) == NULL )
            {
              BUFR_Err_Log( "FXY_List_Expand" );
              (void) FXY_List_Destroy( expList );
              (void) FXY_List_Destroy( OutList );
              return NULL;
            } else
              out_ent = tmp_ent->next;
            }

            /* Restore out_ent to OutList's insertion point. */

            out_ent = ins_ent;

            /*
            * Insert all of the values in 'expList' after 'out_ent'.
            * Repeat this 'Y_val' number of times.
            */
 
            for( i=0; i < Y_val; i++ )
            {
              exp_ent = expList->head->next;
              ins_ent = out_ent;

              while( exp_ent != expList->tail )
              {
    		        /* 12/15/97 LAH: Added argument to allow updating */
		            /*               of modified FXY_List structure   */
                if( FXY_List_Insert( ins_ent, exp_ent->fxy, OutList ) )
                {
                  BUFR_Err_Log( "FXY_List_Expand" );
                  (void) FXY_List_Destroy( expList );
                  (void) FXY_List_Destroy( OutList );
                  return NULL;
                }

                exp_ent = exp_ent->next;
                ins_ent = ins_ent->next;
              }
            }
            /* Expanded FXY list is no longer needed. */

            (void) FXY_List_Destroy( expList );
            if( out_ent == OutList->head )
                out_ent = out_ent->next;
        }
      } else    /* No expansion needed.  Go on to the next entry. */
        out_ent = out_ent->next;
    }

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 4 )
    {
      for( i=0; i < R_Calls; i++ )
            fprintf(BUFR_Cntl.bufr_log, "\t" );
      
      fprintf(BUFR_Cntl.bufr_log, "FXY_List_Expand: Returning " );
      FXY_List_Print( OutList, BUFR_Cntl.bufr_log );
    }
#endif

    R_Calls--;

#if IGNORE_NESTING
    /*
    * JRA022597: If decoding a message and this is not a recursive invocation
    * of this function (R_Calls==-1), filter out any repeated/uncancelled
    * 2-01-YYY and 2-02-YYY descriptors.
    *
    * In other words, if a change data with descriptor (2-01-YYY) value was
    * seen and has not been cancelled with a 2-01-000, toss out any other
    * occurrences of 2-01-YYY.  The same applies to 2-02-YYY descriptors.
    *
    */
    if( R_Calls == -1 && BUFR_ProcType() == TYPE_DECODE )
    {
      c201_flag = 0;
      c202_flag = 0;

      out_ent = FXY_List_First( OutList );

      while( out_ent != OutList->tail )
      {
        if( FXY_IsTableC( out_ent->fxy ) )
        {
          X_val = FXY_X_Value( out_ent->fxy );
          Y_val = FXY_Y_Value( out_ent->fxy );

          if( X_val == 1 )
          {
            if( Y_val != 0 )    /* Change data widths */
            {
              if( c201_flag != 0 )
              {
                /* This is a repeated descriptor; toss it out. */
                tmp_ent = FXY_List_Remove( OutList, out_ent );
                out_ent  = tmp_ent;
              } else    /* This is the first occurrence. */
                 c201_flag = 1;

            } else                /* Cancellation */
            {
              if( c201_flag == 0 )
              {
                /* Descriptor already cancelled; toss it out. */
                tmp_ent = FXY_List_Remove( OutList, out_ent );
                out_ent  = tmp_ent;
              } else    /* First cancellation. */
                c201_flag = 0;

            }
          } else if( X_val == 2 )
          {
            if( Y_val != 0 )    /* Change scale values */
            {
              if( c202_flag != 0 )
              {
                /* This is a repeated descriptor; toss it out. */

                tmp_ent = FXY_List_Remove( OutList, out_ent );
                out_ent = tmp_ent;
              } else    /* This is the first occurrence. */
                c202_flag = 1;

            } else                /* Cancellation */
            {
              if( c202_flag == 0 )
              {
                /* Descriptor already cancelled; toss it out. */
                tmp_ent = FXY_List_Remove( OutList, out_ent );
                out_ent = tmp_ent;
              } else    /* First cancellation. */
                c202_flag = 0;
             
            }
          }
        }
        out_ent = out_ent->next;
      }
    }
#endif

    return OutList;
}
