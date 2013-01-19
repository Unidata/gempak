/*
 * FXY_Print_Recursive - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Print_Recursive -  preforms recursive print of descriptors
 *     such as Table D descriptor component descriptors
 */
/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_Print_Recursivet
 
  DESCRIPTION:         Preforms recursive print of descriptors
                       such as Table D descriptor component descriptors
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    Louis Hembree  05/07/98
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	void FXY_Print_Recursive(FXY_t fxy,  FILE* fp )

*/

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void FXY_Print_Recursive( FXY_t fxy,  FILE* fp)
#else

void FXY_Print_Recursive( fxy, fp)
FXY_t fxy;
FILE* fp;

#endif
{
    FXY_List_t*  exp_list;
    FXY_Entry_t* fxy_entry;
    Descriptor_t* d;
    int X_Val, Y_Val;
    static int R_Calls = -1;    /* For debugging, # recursive calls */
    int n;
    
    R_Calls++;
    for ( n = 0; n < R_Calls; n++)
        fprintf(fp, "               ");    
	
    if( FXY_IsTableD( fxy ) )
    {
      fprintf(fp, "%s = Table D entry in delayed replication", 
          FXY_String( fxy));
      fprintf(fp, " - not expanded\n");
      exp_list = FXY_List_Expand( &fxy, 1 );
      fxy_entry = exp_list->head->next;
      for( n=0; n < exp_list->num_fxys; n++ ) 
      {
        FXY_Print_Recursive(fxy_entry->fxy, fp);
	      /* 051998 LAH code modified and moved to FXY_Print_Recursice */
        fxy_entry = fxy_entry->next;
      }
    } else if( FXY_IsTableB(fxy ) ) 
    {
      fprintf(fp, "%s", FXY_String( fxy) );
      if ( (d = TableB_Get(fxy)) == NULL )
          fprintf( fp, " = Unknown Table B FXY Descriptor\n");
      else
          fprintf( fp," = %s\n", d->description);
       
    } else if( FXY_IsReplicator(fxy ) ) 
    {
      fprintf(fp, "%s =", FXY_String( fxy) );
      X_Val = FXY_X_Value( fxy );
      if ( (Y_Val = FXY_Y_Value( fxy )) == 0 )
      {
	      fprintf(fp, " Delayed replication of %d descriptors", X_Val);
        fprintf(fp, " - can't expand\n");
      } else
      {
        fprintf(fp, " Explicate Replication of %d descriptors", X_Val);
        fprintf(fp, " - not expanded\n");
      }
    } else if ( FXY_IsTableC( fxy ) )
    {
      fprintf(fp, "%s ", FXY_String( fxy)); 
      X_Val = FXY_X_Value( fxy );
      switch( X_Val)
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
           Y_Val = FXY_Y_Value(fxy);
           if ( Y_Val == 0 ) 
           {
             fprintf( fp, " Substituted values operator\n");
           } else {
             fprintf( fp, " Substituted values marker operator\n");			
           }
           break;
        case 24:
           Y_Val = FXY_Y_Value(fxy);
           if ( Y_Val == 0 )
           {
             fprintf( fp, " First order statistical values follow\n");
           } else {
             fprintf( fp, " First order statistical values marker operator\n");			
           }
           break;
        case 25:
           Y_Val = FXY_Y_Value(fxy);
           if ( Y_Val == 0 )
           {
             fprintf( fp, " Difference statistical values follow\n");
           } else {
             fprintf( fp, " Difference statistical values marker operator\n");			
           }
           break;
        case 32:
           Y_Val = FXY_Y_Value(fxy);
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
           Y_Val = FXY_Y_Value(fxy);
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
    }
    R_Calls--;
}
