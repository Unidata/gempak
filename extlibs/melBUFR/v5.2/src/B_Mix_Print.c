/*
* B_Mix_Print VERSION: %I%  %E% %T%
*
* PURPOSE:  Prints contents of Data_MixVal_t array.
*
* WRITTEN BY: Louis Hembree
*             NRL Monterey
*  ARH/SAIC  032102  Made routine ruturn void.
*/

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int B_Mix_Print(Data_MixVal_t *bufr_rec,  int num)

#else
 
int B_Mix_Print(bufr_rec,  num)
Data_MixVal_t *bufr_rec;
int num;
#endif
{
   extern BUFR_Cntl_t BUFR_Cntl;
   int i;
   FILE *fp;
   Data_MixVal_t *rec_pr;
   
   fp = BUFR_Cntl.bufr_log;
   rec_pr = bufr_rec;
   for ( i = 1; i<= num; i++)
   {
     if ( rec_pr->Val_Type == DT_STRING ) 
     {
       fprintf(fp, "%d DT_STRING => \"%s\"\n", i, rec_pr->Val.string );
     } else if(rec_pr->Val_Type == DT_INT)
     {
        fprintf(fp, "%d DT_INT => %13d\n", i, rec_pr->Val.int_number );
     } else if(rec_pr->Val_Type == DT_SHORT)
     {
        fprintf(fp, "%d DT_SHORT => %13d\n", i, rec_pr->Val.short_num );
     } else if(rec_pr->Val_Type == DT_FLOAT)
     {
        fprintf(fp, "%d DT_FLOAT => %13.4f\n", i, rec_pr->Val.ffloat );
     } else
     {
        fprintf(fp, "%d DOUBLE => %13.4f\n", i, rec_pr->Val.number );
     }
     rec_pr = rec_pr+1;
   }   
   return (0);
}
