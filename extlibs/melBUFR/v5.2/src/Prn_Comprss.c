/*
 * print_compressed - VERSION: %I%  %E% %T%
 */
/*
 * print_compressed - Print the contents of a BUFR_Val_t to the
 * given file descriptor.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
#if DEBUG_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

#if PROTOTYPE_NEEDED

void print_compressed( FILE* fp, int range1, int range2, BUFR_Val_t* D_con, 
			int num_fxys, int no_sets, FXY_t* whole_fxys )

#else

void print_compressed( fp, range1, range2, D_con, num_fxys, no_sets, 
			whole_fxys )
FILE* fp;
int range1;
int range2;
BUFR_Val_t* D_con;
int        num_fxys;    
int        no_sets;    
FXY_t* whole_fxys;

#endif
{

    Descriptor_t* d;
    int n, j;
    int PrintDes;
    int num_part;
    int inum;

    PrintDes = 1;
    num_part = num_fxys + 1;
    inum = 0;
 

    for(n = range1; n <= range2; n++){
      for(j = 0; j < num_fxys; j++){
        if( FXY_IsTableC( whole_fxys[j] ) ) continue;
        fprintf( fp, "%s", FXY_String( whole_fxys[j] ) );

        fprintf( fp, " = " );
        if ( D_con[inum + (n * num_part)].missing_flag == 0 )
            fprintf( fp, "MISSING VALUE" ); 
        else if( D_con[inum + (n * num_part)].Val_Type == DT_STRING )
        {
            printf( "\"%s\"", D_con[inum + (n * num_part)].Val.string );
        }
        else if(D_con[inum + (n * num_part)].Val_Type == DT_INT)
        {
            fprintf( fp, "%13d", D_con[inum + (n * num_part)].Val.int_number );
        }
        else if(D_con[inum + (n * num_part)].Val_Type == DT_SHORT)
        {
            fprintf( fp, "%13d", D_con[inum + (n * num_part)].Val.short_num );
        }
        else if(D_con[inum + (n * num_part)].Val_Type == DT_FLOAT)
        {
            fprintf( fp, "%13.4f", D_con[inum + (n * num_part)].Val.ffloat );
        }
        else
        {
            fprintf( fp, "%13.4f", D_con[inum + (n * num_part)].Val.number );
        }


#if DEBUG_PRINT
    	if( BUFR_DebugLevel() > 1 )
    	{
          fprintf( BUFR_Cntl.bufr_log,
	    " (%2d/%6d/%2d)", FXY_Get_Scale( whole_fxys[j] ),
            FXY_Get_RefVal(whole_fxys[j]), FXY_Get_DataWidth(whole_fxys[j]) );
    	}
#endif



    	if( PrintDes )
    	{
          if( FXY_IsTableB( D_con[inum + (n * num_part)].FXY_Val ) )
          {
            /* Print FXY description. */

            if( (d = TableB_Get( whole_fxys[j] )) == NULL )
                fprintf( fp, " = Unknown Table B FXY value\n" );
            else
                fprintf( fp, " = %s [%s]\n", d->description, d->units );
          }
          else if( FXY_IsTableC( whole_fxys[j] ) )
          {
            switch( FXY_X_Value( whole_fxys[j] ) )
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

                default:
                    fprintf( fp, " = Invalid Table C operator\n" );
            }
          }
          else
            fprintf( fp, " = Invalid FXY value\n" );
    	}
    	else
          fprintf( fp, "\n" );

 	inum++;
      } /* end of j loop */
    } /* end of range loop */

}

