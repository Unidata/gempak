#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Missing_Value_Replace( double* d, FXY_t fxy)

#else

int Missing_Value_Replace( d, fxy)
double* d;
FXY_t fxy;
#endif
{
    Units_t type;
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    type = FXY_UnitsType( fxy );
    if ( type == CODE_TABLE )
    {
      if ( BUFR_Cntl.Replace_Code_Table_Missing )
      {
	  *d = BUFR_Msg.Missing_Value;
      }
    } else if( type != CCITT_IA5)
    {
	if ( BUFR_Cntl.Replace_Missing_Values )
	{
	  *d = BUFR_Msg.Missing_Value;  
	}
    }
   return(0);
}	
  
