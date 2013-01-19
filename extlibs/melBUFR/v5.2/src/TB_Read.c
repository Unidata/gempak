/*
 * TableB_Read - VERSION: %I%  %E% %T%
 */
/*
 * TableB_Read - Read descriptors for Code Table B from the given filename.
 * Return 1 on error, else 0.
 */

#include <mel_bufr.h>
extern TableB_t*  TableB;
extern BUFR_Cntl_t BUFR_Cntl;

/* 092997  LAH:  Added include to get character types 
 *
 * 
 * 022398  LAH:  - Add reference to BUFR_Cntl to control printing of 
 *               of warning messages.
 *               - Modified to remove conditional compile to 
 *               allow redefinition of table entries.  Now controlled by
 *               BUFR_Cntl.Dup_Tab_Entry_Allow.  1 = allow duplication, 
 *               0 = disallow duplication
 * 022498  LAH:  - Added & changed prints to bufr_log
 *
 * 083098  VLP: - Check to see if duplicate table B values are identical to
 *			Master table B values.  If so, don't add duplicate
 *			table B item.  If item is an actual local Table B
 *			item, set local_flag to 1.
 */
#include <ctype.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         TableB_Read
 
  DESCRIPTION:         Read descriptors for Code Table B from the given 
			filename.  Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin

  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int TableB_Read( char* FileName )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FileName              Char        In          Input file name
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Filename is NULL      Write error message to buffer
                        Return with an error
  File doesn't open     Write error message to buffer
                        Return with an error
  No F, X, or Y         Write error message to buffer
                        Return with an error
  No descriptor         Write error message to buffer
                        Return with an error
  No units descriptor   Write error message to buffer
                        Return with an error
  Bad call To           Write error message to buffer
  TableB_Put            Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Write error message to a buffer
  BUFR_IsComment        Check to see if line is a BUFR table comment line
  BUFR_strdup           Places a string into a pointer
  FXY_Pack		Returns packed values of F, X, and Y
  FXY_IsTableB		Returns a 1 if given descriptor is in Table B
  ValStack_Init		Initializes a value stack
  TableB_GetUnits	Returns a units value
  ValStack_Destroy	Destroys a value stack
  free		Frees memory
  TableB_Put		Adds a descriptor to the end of TableB
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  TableB        TableB_t        External table B array
  BUFR_Msg	BUFR_Msg_t	External Bufr Message
  fp            FILE            File pointer
  buf           char            Character array for diagnostic messages
  tokbuf        char            Character array
  valp          char            Pointer to value
  line		int		loop counter
  f		int		The F value of an FXY
  x		int		The x value of an FXY
  y		int		The y value of an FXY
  scale		int		The scaling factor
  ref		int		The reference value
  dw		int		The data width
  units		char		Character representation of units
  description	char		Description of each Table B item
  dt		Descriptor_t	Structure of fxys
  tbe		TabeB_Entry_t	TableB entry item
  tbe_prev	TabeB_Entry_t	TableB previous entry item
 
  METHOD:
        If the input filename is empty then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
        If there is an open error then
          Perform BUFR_Err_Set to write error message
          Return with error
        Endif
        Loop while reading the file until the file is empty
          If the current line is a comment then
            drop to the bottom of the loop
          Endif
          Perform strcpy to copy the line buf to tokbuf
          Loop on the length of the line
            If the end of the line has whitespace then
              remove the whitespace
            Endif
          Endloop
          Perform strtok to get the f value in a line
          If there is no f value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Else
            Perform atoi to place the f value in f
          Endif
          Perform strtok to get the x value in a line
          If there is no x value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Else
            Perform atoi to place the x value in x
          Endif
          Perform strtok to get the y value in a line
          If there is no y value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Else
            Perform atoi to place the y value in y
          Endif
          Perform strtok to get the scale value in a line
          If there is no scale value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Else
            Perform atoi to place the scale value in scale
          Endif
          Perform strtok to get the reference value in a line
          If there is no reference value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Else
            Perform atoi to place the reference value in ref
          Endif
          Perform strtok to get the data width value in a line
          If there is no data width value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Else
            Perform atoi to place the data width value in dw
          Endif
	  If there is an error performing either FXY_Pack or FXY_IsTableB then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Endif
	  Place data width and scale into the TableB structure
	  If there is an error performing ValStack_Init then
            Close the file
            Return with error
          Else
	    Set the tail member of the reference value stack to the default val
	  Endif
	  If the units is a NULL value then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
          Endif
	  Loop to find end of leading whitespace in units
	    If character is a non whitespace break
	  End Loop
	  If the character is an end-of-line or a NULL character then
            Perform BUFR_Strdup to place 3 question marks in the table
          Else
            Perform BUFR_Strdup to place the string in the table
          Endif
	  Place units type, units, and description into the TableB structure
          if redefinition allowed then
	     Loop to find the Table B entry that needs to be redefined
  	     Remove the Table B entry and perfrom free to free up memory
	     Deallocate the descriptor
	     if warning flag set then
	        print warning to log file
	     Endif
          else
             Perform BUFR_Err_Set to write error message
             Close the file
             Return with error
	  Endif
	  If while performing TableB_Put is an error then
            Perform BUFR_Err_Set to write error message
            Close the file
            Return with error
	  Endif
	Endloop
	Close the file
	Return with no error
	    
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    030796 LAH: Added code to free temporary descriptor variable
 
    092997  LAH: Added char casts to correct Linux warning 
                 Added include to get character types
                 Removed definition of unused varibale BUFR_Msg

    100997  LAH: Added ulong_t cast in isspace 

..............END PROLOGUE...................................... */
#define MAX_BUF 1024

#if PROTOTYPE_NEEDED

int TableB_Read( char* FileName )

#else

int TableB_Read( FileName )
char*       FileName;

#endif
{

    FILE* fp;
    int   line;
    char  buf[MAX_BUF];
    char  tokbuf[MAX_BUF];  /* copy of buf[] for strtok() to work on */
    int   f, x, y, scale, ref, dw;
    char  *valp;

    char* units;
    char* description;

    Descriptor_t dt;

    TableB_Entry_t *tbe, *tbe_prev;

    /*  092997  LAH: Added cast to correct Linux warning */
    if( FileName == NULL && *FileName == (char) NULL )
    {
        BUFR_Err_Set( "TableB_Read", "NULL Table B filename given" );
	fprintf(BUFR_Cntl.bufr_log, 
	     "TableB_Read: NULL Table B filename given\n");
        return 1;
    }

    if( (fp = fopen( FileName, "r" )) == NULL )
    {
        sprintf( buf, "Can't open Table B file \"%s\"", FileName );
        BUFR_Err_Set( "TableB_Read", buf );
        fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
        return 1;
    }
    
    fprintf(BUFR_Cntl.bufr_log, "Reading Table B (%s)\n", FileName );

    for( line=1; fgets( buf, MAX_BUF, fp ) != NULL; line++ )
    {
        if( BUFR_IsComment( buf ) )
            continue;

        /*
         * Since strtok() mangles the string it works on, let strtok() use
         * a copy of buf.
         */

        strcpy( tokbuf, buf );

        /*
         * Trim trailing whitespace in 'tokbuf' (this makes it easier
         * to get the description later on).
         */

        /*  092997  LAH: Added char cast to correct Linux warning */
        /* 100997 LAH: Added ulong_t cast */
        for( valp=&tokbuf[strlen(tokbuf)-1]; isspace( (ulong_t) *valp ); valp-- )
            *valp = (char) NULL;

        /* Get F, X, and Y portion and compute table location */

        if( (valp = strtok( tokbuf, " ;" )) == NULL )
        {
            sprintf(buf, "%s, Line %d: Missing F descriptor", FileName, line);
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }
        else
            f = atoi( valp );

        if( (valp = strtok( NULL, " ;" )) == NULL )
        {
            sprintf(buf, "%s, Line %d: Missing X descriptor", FileName, line);
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }
        else
            x = atoi( valp );

        if( (valp = strtok( NULL, " ;" )) == NULL )
        {
            sprintf(buf, "%s, Line %d: Missing Y descriptor", FileName, line);
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }
        else
            y = atoi( valp );

        if( (valp = strtok( NULL, " ;" )) == NULL )
        {
            sprintf(buf, "%s, Line %d: Missing Scale value", FileName, line);
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }
        else
            scale = atoi( valp );

        if( (valp = strtok( NULL, " ;" )) == NULL )
        {
            sprintf( buf, "%s, Line %d: Missing Reference Value",
                FileName, line );
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }
        else
            ref = atoi( valp );

        if( (valp = strtok( NULL, " ;" )) == NULL )
        {
            sprintf( buf, "%s, Line %d: Missing Data Width value",
                FileName, line );
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }
        else
            dw = atoi( valp );

        if((dt.fxy_value=FXY_Pack(f,x,y))==(FXY_t)-1 || !FXY_IsTableB(dt.fxy_value))
        {
            sprintf( buf,
                "%s, Line %d: Invalid Table B descriptor (%d-%02d-%03d)",
                FileName, line, f, x, y );
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }

        dt.data_width = dw;
        dt.scale      = scale;

        /* Create (empty) stack of redefined reference values. */

        if( ValStack_Init( &dt.RefValStack ) )
        {
            (void) fclose( fp );
            return 1;
        }
        else
        {
            /*
             * Set tail member of reference value stack to the default
             * value so that RefValStack->head->next->val always has
             * a valid value.
             */

            dt.RefValStack.tail->val = ref;
        }

        if( (units = strtok( NULL, " ;\n" )) == NULL )
        {
            sprintf( buf, "%s, Line %d: Missing Units description",
                FileName, line );
            BUFR_Err_Set( "TableB_Read", buf );
            fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);
            (void) fclose( fp );
            return 1;
        }

        /*
         * strok() placed a null byte after 'units.'  We want to ignore
         * whitespace before the description but not within it.  Since
         * strtok() doesn't provide a convenient way to deal with this,
         * set 'description' to point to the first non-whitespace after
         * the null byte at the end of 'units.' Trailing whitespace has
         * already been trimmed.
         */

        /* Ignore leading whitespace. */

        /*  092997  LAH: Added cast to correct Linux warning */
        for( valp=units+strlen(units)+1; *valp != (char) NULL; valp++ )
        {
            /* 100997 LAH: Added ulong_t cast */
            if( !isspace( (ulong_t) *valp ) )
                break;
        }

        /*  092997  LAH: Added cast to correct Linux warning */
        if( *valp == '\n' || *valp == (char) NULL )
            description = BUFR_strdup( "???" );
        else
            description = BUFR_strdup( valp );

        dt.units_type  = TableB_GetUnits( units );
        dt.units       = BUFR_strdup( units );
        dt.description = BUFR_strdup( description );
	dt.local_flag  = 0; /* false, because this is a master table element */

#if DEBUG_PRINT
        if( BUFR_DebugLevel() > 8 )
        {
          fprintf(BUFR_Cntl.bufr_log, 
	      "%d-%02d-%03d  %2d  %9d  %3d  %-16s  %s\n",
              f, x, y, scale, ref, dw, units, description );
        }
#endif

        /*  Deallocate memory used to description during copy (+3/7/96)  */
        /*  memory is malloced by the BUFR_strdup function               */
        /*  instead of using BUFR_strdup can the strcpy function be used */
        /*  to create the intermediate description variable and then     */
        /*  BUFR_strdup to put into the dt structure.                    */

        free( (void*) description );

        /* Check for descriptor duplication. */

        if( TableB_Get( dt.fxy_value ) != NULL )
        {
	   /*
	    * LAH 022398:  Modified to remoce conditional compile to 
	    * allow redefinition of table entries.  Now controlled by
	    * BUFR_Cntl.Dup_Tab_Entry.  1 = allow duplication, 
	    * 0 = disallow duplication
	    */
            if (BUFR_Cntl.Dup_Tab_Entry)
	    {
              if (BUFR_Cntl.Dup_Tab_Entry_Warn)
	      {
	       fprintf(BUFR_Cntl.bufr_log, 
	         " found a duplicate Table B at %d with %1d-%02d-%03d\n",
	         line, f, x, y);
	      }

/* FNMOC has a habit of using the same descriptor in their local tables.
   So check to make sure that the data width and scale and reference value
   are the same.  If so, use the master table descriptor.  */

	      FXY_Get_Values( dt.fxy_value, &scale, &ref, &dw);
	      if(ref == dt.RefValStack.tail->val && 
		scale == dt.scale && dw == dt.data_width) continue;

	      dt.local_flag  = 1; /* true, because this is a local table */
              /*******************************************************
              * Remove this Table B entry so that the redefined one will
              * be added later.
              *********************************************************/

              /* Find Table B linked list entry and the one before it. */

              tbe_prev = TableB->head;
              tbe      = TableB->head->next;

              while( tbe->item->fxy_value != dt.fxy_value )
              {
                tbe_prev = tbe_prev->next;
                tbe      = tbe->next;
              }

              /*
              * Remove the Table B entry so that the redefined one will be
              * added later.
              */

              tbe_prev->next = tbe->next;

              (void) ValStack_Destroy( &tbe->item->RefValStack );

              /* Destroy descriptor (item). */

              free( (void*) tbe->item->units );
              free( (void*) tbe->item->description );
              free( (void*) tbe->item );

              /* Deallocate this descriptor. */

              free( (void*) tbe );
            } else {
	      sprintf( buf,
                "%s, Line %d: Duplicate Table B descriptor (%d-%02d-%03d)",
                FileName, line, f, x, y );

              BUFR_Err_Set( "TableB_Read", buf );
              fprintf(BUFR_Cntl.bufr_log, "TableB_Read: %s \n",  buf);

              (void) fclose( fp );
              return 1;
	    }
        }

        /* Add descriptor to the end of the descriptor list. */

        if( TableB_Put( dt ) )
        {
            BUFR_Err_Set( "TableB_Read", "Can't add descriptor to list" );
            fprintf(BUFR_Cntl.bufr_log, 
	       "TableB_Read: Can't add descriptor to list\n");
            (void) fclose( fp );
            return 1;
        }
    }

    (void) fclose( fp );

    return 0;
}
