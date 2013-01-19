        SUBROUTINE MA_DCD1 ( mszrpt, marrpt, ipt, iret )
C************************************************************************
C* MA_DCD1						                *
C*								        *
C* This subroutine gets the length of each of the sections 1-5 in the   *
C* WMO FM13 or CMAN report.  These data are used to determine which     *
C* sections contain groups.  If a section has groups, the appropriate   *
C* routine is called to decode the groups.  Section 1 must be included  *
C* in a report or else the report is invalid.                           *
C*			 					        *
C* MA_DCD1  ( MSZRPT, MARRPT, IPT, IRET )   			        *
C*								        *
C* Input parameters:						        *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*      MARRPT          CHAR*           Report array                    *
C*								        *
C* Input and Output parameters:						*
C*	IPT		INTEGER		Pointer to groups in report; on *
C*					input, points ot space before   *
C*					the i(R)i(x)hVV group           *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP      8/96    Added logic to check the error return   *
C*                              variable from MA_LSCB                   *
C* D. Kidwell/NCEP	4/97	Cleaned up code and documentation       *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C* 
        CHARACTER*(*)   marrpt
C------------------------------------------------------------------------
        iret = 0
C
C*      Determine the length of sections 1-5 in report.
C
        CALL  MA_LSCB ( mszrpt, marrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
C*      Decode groups in section 1 of report.
C
        CALL  MA_SC1B ( marrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
        IF ( lsec2 .gt. 0 ) THEN
C
C*          Decode the groups in section 2 of report.
C
            ipt = isec2
            CALL  MA_SEC2 ( marrpt, ipt, jret )
        END IF
C
        IF ( lsec3 .gt. 0 ) THEN
C
C*          Decode the groups in section 3 of report.
C
            ipt = isec3
            CALL  MA_SEC3 ( marrpt, ipt, jret )
        END IF
C
        IF ( lsec5 .gt. 0 ) THEN
C
C*          Decode the groups in section 5 of report.
C
            ipt = isec5
            CALL  MA_SEC5 ( marrpt, ipt, jret )
        END IF
C*
	RETURN
        END
