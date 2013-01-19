/*
 * Get_BUFR_Log_File - VERSION: %I%  %E% %T%
 */
/*
 * Get_BUFR_Log_File:  Used to get name of the  BUFR log file.
 */
#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;
 
 
#if PROTOTYPE_NEEDED

char *Get_BUFR_Log_File( void )

#else

char *Get_BUFR_Log_File( )

#endif
{
    
    return (BUFR_Cntl.BUFR_Log_File);
}
