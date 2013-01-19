/*
 * BUFR_Val_Print - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Val_Print - Print the contents of a BUFR_Val_t to the
 * given file descriptor.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 * 051998 LAH:  Corrected print of "missing value"
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

void BUFR_Val_Print( BUFR_Val_t BV, int PrintDes, int PrintAFs, FILE* fp )

#else

void BUFR_Val_Print( BV, PrintDes, PrintAFs, fp )
BUFR_Val_t BV;
int        PrintDes;    /* Print FXY description. */
int        PrintAFs;    /* Print associated field information. */
FILE*      fp;

#endif
{
    char fmt[100];
    int i, Y_Val;
    int u_scale;

    Descriptor_t* d;

    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "%s", FXY_String( BV.FXY_Val ) );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 1 )
    {
      fprintf( BUFR_Cntl.bufr_log,
          " (%2d/%6d/%2d)", FXY_Get_Scale( BV.FXY_Val ),
      FXY_Get_RefVal( BV.FXY_Val ), FXY_Get_DataWidth( BV.FXY_Val ) );
    }
#endif

    fprintf( fp, " = " );
    
    if ( BV.Val_Type == DT_STRING ) 
    {
      fprintf( fp, "\"%s\"", BV.Val.string );
    } else if ( FXY_IsTableB(BV.FXY_Val)) {
      if ( BV.missing_flag == 0 )
      {
        if ( FXY_UnitsType( BV.FXY_Val ) != CODE_TABLE)
        {
          fprintf( fp, "MISSING VALUE" ); 
        } else {
          if (BUFR_Cntl.Replace_Code_Table_Missing)
          {
            fprintf( fp, "MISSING VALUE" ); 
          } else {
            if(BV.Val_Type == DT_INT)
            {
              fprintf( fp, "%13d", BV.Val.int_number );
            } else if(BV.Val_Type == DT_SHORT)
            {
              fprintf( fp, "%13d", BV.Val.short_num );
            }
          }
        }
      } else if(BV.Val_Type == DT_INT)
      {
        fprintf( fp, "%13d", BV.Val.int_number );

      } else if(BV.Val_Type == DT_SHORT)
      {
        fprintf( fp, "%13d", BV.Val.short_num );

      } else if(BV.Val_Type == DT_FLOAT)
      {
        /* 052302 modified print to use scale value to print sig digits */
        u_scale = BV.Val_Scale;
        if ( BV.Val_Scale < 0) u_scale = 0;
        sprintf(fmt,"%s13.%df", "%", u_scale );
        fprintf( fp, fmt, BV.Val.ffloat );

      } else
      {
        /* 052302 modified print to use scale value to print sig digits */
        u_scale = BV.Val_Scale;
        if ( BV.Val_Scale < 0) u_scale = 0;
        sprintf(fmt,"%s13.%df", "%", u_scale );
        fprintf( fp, fmt, BV.Val.number );
      }
    }
    if( PrintDes )
    {
        if( FXY_IsTableB( BV.FXY_Val ) )
        {
            /* Print FXY description. */

            if( (d = TableB_Get( BV.FXY_Val )) == NULL )
                fprintf( fp, " = Unknown Table B FXY value\n" );
            else
                fprintf( fp, " = %s [%s]\n", d->description, d->units );
        } else if( FXY_IsTableC( BV.FXY_Val ) )
        {
            switch( FXY_X_Value( BV.FXY_Val ) )
            {
                case 1:
                    fprintf( fp, " = Change data width operator\n" );
                    break;

                case 2:
                    fprintf( fp, " = Change scale operator\n" );
                    break;

                case 3:
                    fprintf( fp, " = Change reference values operator\n" );
                    break;

                case 4:
                    fprintf( fp, " = Add associated field operator\n" );
                    break;

                case 5:
                    fprintf( fp, " = Signify character operator\n" );
                    break;

                case 6:
                    fprintf( fp, " = Signify data width for local descriptor operator\n" );
                    break;

		case 22:
		    fprintf( fp, " Quality information follows\n");
		    break;
		    
		case 23:
		    Y_Val = FXY_Y_Value(BV.FXY_Val);
		    if ( Y_Val == 0 )
		    {
		      fprintf( fp, " Substituted values operator\n");
		    } else {
		      fprintf( fp, " Substituted values marker operator\n");			
		    }
		    break;
		case 24:
		    Y_Val = FXY_Y_Value(BV.FXY_Val);
		    if ( Y_Val == 0 )
		    {
		      fprintf( fp, " First order statistical values follow\n");
		    } else {
		      fprintf( fp, " First order statistical values marker operator\n");			
		    }
		    break;
		case 25:
		    Y_Val = FXY_Y_Value(BV.FXY_Val);
		    if ( Y_Val == 0 )
		    {
		      fprintf( fp, " Difference statistical values follow\n");
		    } else {
		      fprintf( fp, " Difference statistical values marker operator\n");			
		    }
		    break;
		case 32:
		    Y_Val = FXY_Y_Value(BV.FXY_Val);
		    if ( Y_Val == 0 )
		    {
		      fprintf( fp, " Replaced/retained values follow\n");
		    } else {
		      fprintf( fp, " Replaced/retained value marker operator\n");			
		    }
		    break;
		case 35:
		    fprintf( fp,  " Cancel backward data reference\n");
		    break;
		case 36:
		    fprintf( fp, "Define data present bit-map\n");
		    break;
		case 37:
		    Y_Val = FXY_Y_Value(BV.FXY_Val);
		    if ( Y_Val == 0 )
		    {
		      fprintf( fp, " Use defined data present bit-map\n");
		    } else {
		      fprintf( fp, " Cancel use defined data present bit-map\n");			
		    }
		    break;
		
                default:
                    fprintf( fp, " = Invalid Table C operator\n" );
            }
        } else if( FXY_IsTableD( BV.FXY_Val ) )
        {
            fprintf( fp, " = Table D FXY value\n" );

        } else
            fprintf( fp, " = Invalid FXY value\n" );

    } else
        fprintf( fp, "\n" );


    if( PrintAFs )
    {
        /* Print associated field information. */

        if( BV.num_AFs == 0 )
        {
            fprintf( fp, "    No associated fields\n" );
        }
        else
        {
            fprintf( fp, "    AF/Sig = " );

            for( i=0; i < BV.num_AFs; i++ )
            {
                fprintf( fp, "%f/%d ", BV.AF[i], BV.AF_sig[i] );
            }
            fprintf( fp, "\n" );
        }
    }
}

