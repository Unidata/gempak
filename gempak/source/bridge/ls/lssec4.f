        SUBROUTINE  LS_SEC4 ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_SEC4                                                              *
C*                                                                      *
C* This subroutine calls the routine to decode the section 4 cloud      *
C* data groups N'C'H'H'C(t) in the WMO FM12 report.                     *
C*                                                                      *
C* LS_SEC4  ( LSFRPT, IPT, IRET ) 	                                *
C*                                                                      *
C* Input parameters:                                                    *
C*	LSFRPT          CHAR*           Length of section 4             *
C*      IPT             INTEGER         Pointer to start of section 4   *
C*                                                                      *
C* Output parameters:                                                   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP      8/99                                            *
C* R. Hollern/NCEP      9/99   Decode at most 2 cloud layers            *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C------------------------------------------------------------------------
        iret = 0
        lvl = 0
C
        iend = ipt + lsec4 - 1
C
        DO WHILE ( ipt .lt. iend )
C
           IF ( lsfrpt ( ipt:ipt ) .eq. ' ' ) THEN
C
C*             Decode the cloud data group.
C             
               lvl = lvl + 1
               IF ( lvl .gt. 2 ) RETURN
               ipt = ipt + 1
               CALL  LS_CLD4 ( lsfrpt, ipt, lvl, jret )
           END IF
C
           ipt = ipt + 1   
C
        END DO
C*
        RETURN
        END
