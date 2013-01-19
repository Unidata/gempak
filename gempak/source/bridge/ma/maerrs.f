        SUBROUTINE MA_ERRS ( ierrno, marrpt, iret )
C************************************************************************
C* MA_ERRS                                                              *
C*								        *
C* This subroutine writes an error message to the decoder log file.     *
C* The value of IERRNO will be used to choose which message to write.   *
C*								        *
C* MA_ERRS  ( IERRNO, MARRPT, IRET )                                    *
C*								        *
C* Input parameters:						        *
C*      IERRNO         INTEGER           Error message number           *
C*      MARRPT         CHAR*             Current raw report             *
C*                                                                      *
C* Output parameters:						        *
C*      IRET		INTEGER	         Return code                    *
C*                                         0 = Normal return            *
C*                                                                      *
C**                                                                     *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       9/96   Corrected index problem                 *
C* D. Kidwell/NCEP	 4/97	Cleaned up code				*
C* D. Kidwell/NCEP	10/97	Modified log messages 			*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C* 
        CHARACTER*(*)   marrpt
C------------------------------------------------------------------------
        iret = 0
        loglvl = 2
C
C*      Don't write log message if CMAN station problem in SHVX16.
C
        IF ( buhd ( 1:6 ) .eq. 'SHVX16' ) RETURN
C
        logmsg =  seqnum // buhd // orign // btime
        CALL DC_WLOG ( loglvl, 'MA', 4, logmsg (1:28), ier )
        CALL DC_WLOG ( loglvl, 'MA', ierrno, marrpt (1:59), ier )
C*
        RETURN
        END
