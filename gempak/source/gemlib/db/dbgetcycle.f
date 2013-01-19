        SUBROUTINE DB_GETCYCLE  ( cycle, iret )
C************************************************************************
C* DB_GETCYCLE                                                          *
C*                                                                      *
C* This subroutine puts DB info into common.                            *
C*                                                                      *
C* DB_GETCYCLE  ( DBINFO, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      DBINFO          CHAR*          DB info                          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	04/11                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   cycle
C*
C------------------------------------------------------------------------
C*
        CALL ST_LSTR ( dbtime, ldbtime, ier )
        IF ( ldbtime .eq. 0) THEN 
           iret = -1
        ELSE
           cycle = dbtime ( :ldbtime)
           iret = 0   
        END IF
        CALL ST_NULL (cycle, cycle, lstr, ier)
        RETURN
        END
