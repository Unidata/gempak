        SUBROUTINE LS_SEC5( lszrpt, lsfrpt, ipt, iret )
C************************************************************************
C* LS_SEC5							        *
C*								        *
C* This subroutine calls the routines to decode the groups in section 5 *
C* of the WMO FM12 report.  This section starts with the 555 group.     *
C* The number of groups in the section will vary.                       *
C*								        *
C* LS_SEC5  ( LSZRPT, LSFRPT, IPT, IRET )   			        *
C*							                *
C* Input parameters:						        *
C*      LSZRPT          INTEGER         Report length                   *
C*      LSFRPT          CHARACTER       Report array                    *
C*	LSEC5           INTEGER         Length of section 5             *
C*      ISEC5           INTEGER         Pointer to start of section 5   *
C*      CIVALS(ICSTID)  CHAR*           Report ID                       *
C*								        *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to groups in report     *
C*								        *
C* Output parameters:						        *
C*                                                                      *
C*	IRET		INTEGER		Return code		        *
C*				   	 0 = normal return 	        *
C* 								        *
C* Log:								        *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP       1/98   Cleaned up code                         *
C* A. Hardy/GSC          1/98   Reordered calling sequence, added GEMINC*
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)    lsfrpt
C*
        CHARACTER        rpid*2
C------------------------------------------------------------------------
        iret = 0
C
        iend = isec5 + lsec5 - 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 9' ) THEN
C
C*              The 9YYGG time group is not decoded, since the time 
C*              data is gotten elsewhere.
C
C               ipt = ipt + 1
                RETURN
            END IF
C
C*          A few block 72 U.S. stations report city data.
C
            rpid = civals ( icstid ) (1:2)
C
            IF ( rpid .eq. '72' ) THEN
C
                IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 1' ) THEN
C
                    IF ( lsfrpt ( ipt+5:ipt+5 ) .eq. ' ' ) THEN
C
C*                      City temperature group.
C
                        ipt = ipt + 2
                        CALL LS_CTMP ( lsfrpt, ipt, iret )
                      ELSE IF ( lsfrpt ( ipt+7:ipt+7 ) .eq. ' ' ) THEN
C
C*                      City max/min temperature group.
C
                        ipt = ipt + 1
                        CALL LS_CMMT ( lsfrpt, ipt, iret )
                    END IF
                  ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 0' ) THEN
                    IF ( lsfrpt ( ipt+7:ipt+7 ) .eq. ' ' ) THEN
C
C*                      City max/min temperature group.
                        ipt = ipt + 1
                        CALL LS_CMMT ( lsfrpt, ipt, iret )
                    END IF
                  ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 2' ) THEN
C
C*                  City 24 hour precipitation amount.
                    ipt = ipt + 2
                    iparam = 2
                    CALL LS_PREC ( lsfrpt, iparam, ipt, iret )
                END IF
            END IF
        END DO
C*
	RETURN
        END
