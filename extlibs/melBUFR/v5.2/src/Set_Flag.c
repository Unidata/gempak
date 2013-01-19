/*
 * Set_Flag - VERSION: %I%  %E% %T%
 */
/*
 * Set_Flag:  Used to set BUFR control flags.
 */
#include <mel_bufr.h>
 /*
  * Changes:
  *   051998  LAH:  Changed 0 to No,  and 1 to Yes
  *                 Added flags for printing FXY define in transmittal
  *                 and to dump message type 11
  *   101398  VLP:  Removed space in front of include.
  */
 
#if PROTOTYPE_NEEDED

int Set_Flag( CNTL_OPT_ENUM value )

#else

int Set_Flag( value )
CNTL_OPT_ENUM value;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    
    switch (value){
	case Allow_Auto_FTP:
	  BUFR_Cntl.Auto_FTP = YES; 
	  break;
	case NoAuto_FTP:
	  BUFR_Cntl.Auto_FTP = NO;
	  break;
	case Allow_Dup_Tab_Entry:
	  BUFR_Cntl.Dup_Tab_Entry = YES;
	  break;
	case No_Dup_Tab_Entry:
	  BUFR_Cntl.Dup_Tab_Entry = NO;
	  break;
	case Warn_Dup_Tab_Entry:
	  BUFR_Cntl.Dup_Tab_Entry_Warn = YES;
	  break;
	case No_Warn_Dup_Tab_Entry:
	  BUFR_Cntl.Dup_Tab_Entry_Warn = NO;
	  break;
	case BUFR_LOG_TO_stdout: 
	  BUFR_Cntl.BUFR_Log_Flag = YES;
	  break;
        case BUFR_LOG_TO_bufr_log_pid:
	  BUFR_Cntl.BUFR_Log_Flag = NO;
	  break;
	case Print_New_Table_Entries:
	  BUFR_Cntl.Print_New_Entries = YES;
	  break;
	case Do_Not_Print_New_Table_Entries:
	  BUFR_Cntl.Print_New_Entries = NO;
	  break;
	case Dump_Data_Type_11_Msg:
	  BUFR_Cntl.Dump_Data_Type_11_Msg = YES;
	  break;
	case Do_Not_Dump_Data_Type_11_Msg:
	  BUFR_Cntl.Dump_Data_Type_11_Msg = NO;
	  break;
	case Replace_Code_Table_Missing_Values:
	  BUFR_Cntl.Replace_Code_Table_Missing = YES;
	  break;
	case Do_Not_Replace_Code_Table_Missing_Values:
	  BUFR_Cntl.Replace_Code_Table_Missing = NO;
	  break;
	case Replace_Missing_Values:	  
	  BUFR_Cntl.Replace_Missing_Values = YES;
	  break;
	case Do_Not_Replace_Missing_Values:	  
	  BUFR_Cntl.Replace_Missing_Values = NO;
	  break;
        case Not_All_Numbers_Double:
          BUFR_Cntl.All_Numbers_Not_Double = YES;
          break;
        case All_Numbers_Double:
          BUFR_Cntl.All_Numbers_Not_Double = NO;
          break;
	default:
	  fprintf(stderr, "Set_Flag: Invalid argument\n");
    }
    return(0);
}
