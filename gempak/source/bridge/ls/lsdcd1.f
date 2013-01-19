        SUBROUTINE LS_DCD1( lszrpt, lsfrpt, ipt, iret )
C************************************************************************
C* LS_DCD1						                *
C*								        *
C* This subroutine gets the length of each of the sections 1-5 in the   *
C* WMO FM12 report.  These data are used to determine which sections    *
C* contain groups.  If a section has groups, the appropriate routine is *
C* called to decode the groups.  Sections 0 and 1 should always be      *
C* included in the report.                                              *
C*								        *
C* LS_DCD1  ( LSZRPT, LSFRPT, IPT, IRET )   			        *
C*								        *
C* Input parameters:						        *
C*      LSZRPT          INTEGER         Report length                   *
C*      LSFRPT          CHAR*           Report array                    *
C*								        *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to groups in report; on *
C*                                      input, points to space before   *
C*                                      the i(R)i(x)hVV group           *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	 0 = normal return 	        *
C*                                       1 = problems                   *
C*								        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       8/96   Check error return variable from LS_LSCB*
C* R. Hollern/NCEP       1/98   Changed interface and cleaned up        *
C* R. Hollern/NCEP       2/98   Renamed LS_SC1B to LS_SEC1              *
C* R. Hollern/NCEP       8/99   Added call to LS_SEC4                   *
C************************************************************************
        INCLUDE 'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C------------------------------------------------------------------------
        iret = 0
C
C*      Determine the length of sections 1-5 in report.
C
        CALL  LS_LSCB ( lszrpt, lsfrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
C*      Decode groups in section 1 of report.
C
        CALL  LS_SEC1 ( lszrpt, lsfrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
        IF ( lsec2 .gt. 0 ) THEN
C
C*          Decode the groups in section 2 of report.
C
            ipt = isec2 
            CALL  LS_SEC2 ( lszrpt, lsfrpt, ipt, jret )
        END IF
C
        IF ( lsec3 .gt. 0 ) THEN
C
C*          Decode the groups in section 3 of report.
C
            ipt = isec3
            CALL  LS_SEC3 ( lszrpt, lsfrpt, ipt, jret )
        END IF
C
        IF ( lsec4 .gt. 0 ) THEN
C
C*          Decode the groups in section 4 of report.
C
            ipt = isec4
            CALL  LS_SEC4 ( lsfrpt, ipt, jret )
        END IF
C
        IF ( kcoun .eq. 'US' .and. lsec5 .gt. 0 ) THEN
C
C*          Decode the groups in section 5 of report.
C
            ipt = isec5
            CALL  LS_SEC5 ( lszrpt, lsfrpt, ipt, jret )
        END IF
C
	RETURN
        END
