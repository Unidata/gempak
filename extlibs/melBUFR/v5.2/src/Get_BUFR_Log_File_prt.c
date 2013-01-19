/*
 * Get_BUFR_Log_File_Ptr - VERSION: %I%  %E% %T%
 */
/*
 * Get_BUFR_Log_File_Ptr:  Used to get pointer to BUFR log file.
 */
 #include <mel_bufr.h>
 
 
#if PROTOTYPE_NEEDED

FILE* Get_BUFR_Log_File_Ptr( void )

#else

FILE* Get_BUFR_Log_File_Ptr( )

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    
    return (BUFR_Cntl.bufr_log);
}
