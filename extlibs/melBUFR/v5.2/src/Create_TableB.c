/*
 * 102097 - VERSION: %I%  %E% %T%
 */
/*
 * Create_TableB - Created function to add to local table if data comes 
 * in message.  Return 1 on error, else 0.
 */

/*************************************************************************
*
*    MODIFICATION LOG
*
*    6/19/97
*    VALERIE PASTOR
*    Created function to add to local table if data comes in message
*
*    10/20/97 LAH: Removed unused variable BM
*                  Added void to function prototype
*                  Added include ctype.c for char functions
*                  Added int cast
*    10/21/97 LAH: Added char cast to correct Linux gcc warning 
*
*    02/24/98 LAH:  Added prints to bufr_log file.
 *                  - Add reference to BUFR_Cntl to control printing of 
 *                  of warning messages.
 *                  - Modified to remove conditional compile to 
 *                  allow redefinition of table entries.  Now controlled by
 *                  BUFR_Cntl.Dup_Tab_Entry_Allow.  1 = allow duplication, 
 *                  0 = disallow duplication
 *   05/07/98 LAH:  Added code to concatanate the second line of 
 *                  description for new descriptor
*************************************************************************/

#include <mel_bufr.h>
#include <ctype.h>
extern BUFR_Cntl_t BUFR_Cntl;
extern TableB_t*  TableB;

#define MAX_BUF 1024

#if PROTOTYPE_NEEDED

/* 102097 LAH: Added void to function prototyre */
int Create_TableB( void )

#else

int Create_TableB( )

#endif
{

    BUFR_Val_t bv;

    int   line, n;
    char  buf[MAX_BUF];
    int   f, x, y, scale, ref, dw;
    int full, ret, i;
    char sign, ref_sign;
    char* pr;
    int   sl;
    char units[80];
    char description[80];
    Descriptor_t dt;

    TableB_Entry_t *tbe, *tbe_prev;
 
    /* Get each value (and any associated fields) from the BUFR message. */
    /* 102097 LAH: Added int cast */
    while( (n= (int)BUFR_Get_Value( &bv, 1 )) != BUFR_EOM && n != BUFR_EOF )
    {
      if( n == BUFR_ERROR || n == BUFR_EOF) /* same as if( BUFR_IsError() ) */
      {
        fprintf(BUFR_Cntl.bufr_log,"Error getting decoded value! 1\n");
        printf("Error getting decoded value! 1\n");
 
        /* Print the reason for the error and exit. */
        BUFR_perror( "Create_TableB" );
 
        /* LAH112902 fix for intermitent crash */
        BUFR_Val_free(&bv);
        BUFR_Destroy(1);
        return 1;
      }
      if((FXY_Unpack_Dec(bv.FXY_Val)) < 10)
          continue;

      if((FXY_Unpack_Dec(bv.FXY_Val)) == 10 && (atoi(bv.Val.string )) == 3)
      {
	      ret = Create_TableD();
        if( ret == BUFR_ERROR || ret == BUFR_EOF) 
        {
          printf( "Error getting decoded value! 3\n" );
   
          /* Print the reason for the error and exit. */
 
          BUFR_perror( "Create_Tabled" );
          BUFR_Destroy(1);
          return 1;
        }
        if( ret == BUFR_EOM )
        {
          /* LAH112902 fix for intermitent crash */
          BUFR_Val_free(&bv);
		      n = Set_Status();
		      return 0;
        }
      }
      switch(FXY_Unpack_Dec(bv.FXY_Val))
      {
	      case 10:
	        f = atoi(bv.Val.string );
	        full = 0;
	        break;
	      case 11:
	        x = atoi(bv.Val.string );
	        full++;
	        break;
	      case 12:
	        y = atoi(bv.Val.string );
	        full++;
	        break;
        case 13:
	        strcpy(description, bv.Val.string);
	        full++;
	        break;
        case 14:
          strcat(description, bv.Val.string);
          sl = strlen(description);
          for ( i = sl-1; i>= 0; i--)
          {
            if ( isspace((ulong_t) description[i]) == 0 )
                 break;
            pr--;
          }
          description[i+1] = '\0';
	        full++;
	        break;
        case 15:
	        strcpy(units, bv.Val.string);
	        full++;
	        break;
        case 16:
	        sign = bv.Val.string[0];
	        full++;
          break;
	      case 17:
	        scale = atoi(bv.Val.string);
	        full++;
	        break;
	      case 18:
	        ref_sign = bv.Val.string[0];
          full++;
          break;
        case 19:
          ref = atoi(bv.Val.string);
          full++;
	        break;
        case 20:
          dw = atoi(bv.Val.string);
          full++;
          break;
      }
      if( full == 10)
      { /* filled out an entire table B entry */
        if((dt.fxy_value=FXY_Pack(f,x,y))==(FXY_t)-1 || !FXY_IsTableB(dt.fxy_value))
        {
          sprintf( buf, "Line %d: Invalid Table B descriptor (%d-%02d-%03d)",
                   line, f, x, y );

          BUFR_Err_Set( "Create_TableB", buf );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "Create_TableB", buf);
          /* LAH112902 fix for intermitent crash */
          BUFR_Val_free(&bv);
          return 1;
        }
        dt.data_width = dw;
        if(sign == '-')
            scale = scale * -1;
        dt.scale      = scale;
        if(ref_sign == '-') 
            ref = ref * -1;

        /* Create (empty) stack of redefined reference values. */
 
        if( ValStack_Init( &dt.RefValStack ) )
        {
            /* LAH112902 fix for intermitent crash */
            BUFR_Val_free(&bv);
            return 1;
        } else
        {
            /*
             * Set tail member of reference value stack to the default
             * value so that RefValStack->head->next->val always has
             * a valid value.
             */
 
            dt.RefValStack.tail->val = ref;
        }

        /* 102097 LAH: Added int cast */
        for(i = 1; i < (int)strlen(units); i++)
        {
          /* 102097 LAH: Added ulong_t cast */
          if(isspace((ulong_t) units[i])  && isalpha( (ulong_t) units[i-1]) 
          && isalpha( (ulong_t) units[i+1]))
               units[i] = '_';

          if(isspace( (ulong_t) units[i]))
               break;
        }

        /* 102197 LAH: Added char cast to correct Linux gcc warning */
        units[i] = (char) NULL;
 
        dt.units_type  = TableB_GetUnits( units );
        dt.units       = BUFR_strdup( units );
        dt.description = BUFR_strdup( description );
 


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

        /* Check for descriptor duplication. */

        if( TableB_Get( dt.fxy_value ) != NULL )
        {
          /*
          * LAH 022498:  Modified to remoce conditional compile to 
          * allow redefinition of table entries.  Now controlled by
          * BUFR_Cntl.Dup_Tab_Entry_Allow.  1 = allow duplication, 
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

            /*************************************************************
            * Remove this Table B entry so that the redefined one will be
            * added later.
            ************************************************************/
 
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
            free( (void*) tbe->item );
 
            /* Deallocate this descriptor. */

            free( (void*) tbe );
          }else
          {
            sprintf( buf,
                "Line %d: Duplicate Table B descriptor (%d-%02d-%03d)",
                 line, f, x, y );

            BUFR_Err_Set( "Create_TableB", buf );
            fprintf(BUFR_Cntl.bufr_log, "Create_TableB: %s \n",  buf);

            /* LAH112902 fix for intermitent crash */
            BUFR_Val_free(&bv);
            return 1;
          }
        }

        /* Add descriptor to the end of the descriptor list. */

        if( TableB_Put( dt ) )
        {
          /* LAH112902 fix for intermitent crash */
          BUFR_Val_free(&bv);
          BUFR_Err_Set( "Create_TableB", "Can't add descriptor to list" );
          fprintf(BUFR_Cntl.bufr_log, "Create_TableB: Can't add descriptor to list\n");
          return 1;
        } 
        if (BUFR_Cntl.Print_New_Entries == YES)
        {
	        fprintf(BUFR_Cntl.bufr_log, "%s %2d", FXY_String(dt.fxy_value), 
	           dt.scale);
	        fprintf(BUFR_Cntl.bufr_log, " %8d %2d %15s", 
	           dt.RefValStack.head->next->val, dt.data_width, dt.units);
	        fprintf(BUFR_Cntl.bufr_log, "  %s\n", dt.description);
        }
      }
    }

    n = Set_Status();
    /* LAH112902 fix for intermitent crash */
    BUFR_Val_free(&bv);
    return 0;
}
