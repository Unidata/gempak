/*
 * Get_Val_String  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

char* Get_Val_String (BUFR_Val_t BV)

#else

char* Get_Val_String (BV)
BUFR_Val_t BV;

#endif
{
   char* str;
   int len;
  
   str =NULL;

   if ( BV.Val_Type != DT_STRING ){
     printf(" Get_Val_String: data type not DT_STRING\n");
     BUFR_Err_Log("Get_Val_String");
     return (str);
   }

   len = strlen(BV.Val.string);
   if (len == 0 ||  BV.missing_flag == 0){
     str = (char*) malloc(sizeof(char)*2);
     strcpy(str," ");
   } else {
     str = (char*) malloc(sizeof(char)*(len+1));
     strcpy(str,BV.Val.string);
   }
   
   return (str);
}
