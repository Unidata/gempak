/*
 * Flag_Print - VERSION: %I%  %E% %T%
 */
/*
 * Flag_Print:  Used to print BUFR control flags.
 */
#include <mel_bufr.h>
 /*
  * Changes:
  * 052098 LAH:  Added prints for new flags Print_New_Entries
  *              and Dump_Data_Type_11_Msg
  * 101398 VLP:  removed space in front of include
  */
 
#if PROTOTYPE_NEEDED

void Flag_Print( FILE *fp )

#else

void Flag_Print(fp )
FILE *fp;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    
    fprintf(fp, " <<< Current settings of flags in BUFR_Cntl >>>\n");
    if ( BUFR_Cntl.Auto_FTP )
    {
      fprintf(fp, "    Auto FTP allowed\n");
    } else {
      fprintf(fp, "    Auto FTP not allowed\n");	
    }
    
    if ( BUFR_Cntl.Dup_Tab_Entry)
    {
      fprintf(fp, "    Duplication of table entries allowed\n");
    } else {
      fprintf(fp, "    Duplication of table entries not allowed\n");	
    }
    
    if ( BUFR_Cntl.Dup_Tab_Entry_Warn)
    {
      fprintf(fp, "    Print warnings of duplication of table entries\n");
    } else {
      fprintf(fp, "    No warnings of duplication of table entries printed\n");
    }
    
    if ( BUFR_Cntl.BUFR_Log_Flag )
    {
      fprintf(fp, "    BUFR log written to stdout\n");
    } else {
      fprintf(fp, "    BUFR log written to $MEL_BUFR_LOG/bufr_log.(pid)\n");	
    }
    
    if ( BUFR_Cntl.Print_New_Entries)
    {
      fprintf(fp,"    New Table B & D entries are printed\n");
    } else {
      fprintf(fp,"    New Table B & D entries are not printed\n");
    }

    if ( BUFR_Cntl.Dump_Data_Type_11_Msg)
    {
      fprintf(fp,"    Message containing new table entries dumped\n");
    } else {
      fprintf(fp,"   Message containing new table entries not dumped\n");
    }
    
    if ( BUFR_Cntl.Replace_Code_Table_Missing)
    {
      fprintf(fp,"    Returned Code table entries indicatting missing are\n");
      fprintf(fp, "         replaced by default value for missing\n");
    } else {
      fprintf(fp,"    Returned Code table entries indicatting missing are not\n");
      fprintf(fp, "         replaced by default value for missing\n");
    }
    
    if ( BUFR_Cntl.Replace_Missing_Values)
    {
      fprintf(fp,"    Missing values are replaced by default value for missing\n");
      fprintf(fp, "         NOTE: should be keying off missing value flag \n");
      fprintf(fp, "         and not the actual value\n\n");
    } else {
      fprintf(fp,"    Missing values are not replaced by default value for missing\n\n");
  }
}
