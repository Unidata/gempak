        SUBROUTINE MA_DCD2  ( mszrpt, marrpt, ipt, iret )
C************************************************************************
C* MA_DCD2						                *
C*								        *
C* This subroutine gets the length of each of the sections 1-5 in the   *
C* WMO FM18 buoy report.  These data are used to determine whether      *
C* each of the sections contain groups to decode.  If a section has     *
C* groups, the appropriate routine is called to start the decoding of   *
C* the groups in that section.  Section 0 must be included in a report, *
C* or else the report is invalid.                                       *
C*								        *
C* MA_DCD2  ( MSZRPT, MARRPT, IPT, IRET )            		        *
C*								        *
C* Input parameters:						        *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*      MARRPT          CHAR* 	        Report array                    *
C*      IPT             INTEGER         Pointer to space before the     *
C*                                      i(R)i(x)hVV group               *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       8/96   Added logic to check the error return   *
C*                              variable from MA_LSCD                   *
C* D. Kidwell/NCEP	 4/97	Cleaned up code and documentation       *
C* D. Kidwell/NCEP	10/97	Changed interface                       *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C------------------------------------------------------------------------
        iret = 0
C
C*      Determine the length of sections 1-5 in report.
C
        CALL  MA_LSCD ( mszrpt, marrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
C*      Decode groups in section 1 of report.
C
        IF ( lsec1 .gt. 0 ) CALL  MA_SC1D ( marrpt, jret )
C
C*      Decode the groups in section 2 of report.
C
        IF ( lsec2 .gt. 0 ) CALL  MA_SC2D ( marrpt, jret )
C
C*      Decode the groups in section 3 of report.
C
        IF ( lsec3 .gt. 0 ) CALL  MA_SC3D ( marrpt, jret )
C
C*      Decode the groups in section 4 of report.
C
        IF ( lsec4 .gt. 0 ) CALL  MA_SC4D ( marrpt, jret )
C*
	RETURN
        END
