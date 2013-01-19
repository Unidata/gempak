        SUBROUTINE LS_ERRS ( ierrno, lsfrpt, iret )
C************************************************************************
C* LS_ERRS                                                              *
C*								        *
C* This subroutine writes an error message to the decoder log file.     *
C* The value of IERRNO will be used to choose which message to write.   *
C*								        *
C* LS_ERRS  ( IERRNO, LSFRPT, IRET )                                    *
C*								        *
C* Input parameters:						        *
C*      IERRNO         INTEGER           Error message number           *
C*      LSFRPT         CHAR*             Current raw report             *
C*                                                                      *
C* Output parameters:						        *
C*      IRET		INTEGER	         Return code                    *
C*                                        0 = normal return             *
C*                                                                      *
C**                                                                     *
C* Log:								        *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP	 1/98	Modified log messages; cleaned up code  *
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C------------------------------------------------------------------------
        iret = 0
        loglvl = 2
C
        logmsg =  seqnum // buhd // orign // btime
        CALL DC_WLOG ( loglvl, 'LS', 3, logmsg ( 1:28 ), ier )
        CALL DC_WLOG ( loglvl, 'LS', ierrno, lsfrpt ( 1:59 ), ier )
C*
        RETURN
        END
