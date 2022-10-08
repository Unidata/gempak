	REAL*8 FUNCTION UT_IWBF  ( rval )
C************************************************************************
C* UT_IWBF								*
C*									*
C* This function takes as input an interface value which represents a	*
C* code figure from WMO Code Table 1855 ("Indicator for source and	*
C* units of wind speed") and converts it to a REAL*8 BUFR value which	*
C* represents a flag table value from WMO BUFR Table 0 02 002.  If the	*
C* input value is not a legal code figure from WMO Code Table 1855 (or	*
C* if RMISSD is input!), then the BUFR "missing" value is returned.	*
C*									*
C* UT_IWBF  ( RVAL )							*
C*									*
C* Input parameters:							*
C*	RVAL		REAL		Interface value	representing	*
C*					a code figure from 		*
C*					WMO Code Table 1855		*
C*									*
C* Output parameters:							*
C*	UT_IWBF		REAL*8		BUFR value representing		*
C*					a flag table value from		*
C*					WMO BUFR Table 0 02 002		*
C*					which has bitwidth of 4.	*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/02						*
C* C. Caruso Magee/NCEP 03/06           Modify/add comments and replace *
C*                                      hardcoded 0 02 002 flag table   *
C*                                      values with new bufrlib function*
C*                                      PKFTBV.                         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BUFR.CMN'
 
        REAL*8          PKFTBV
C*-----------------------------------------------------------------------
	iret = 0
C
	isws = INT ( rval )
C
C*      Convert code table 1855 values to flag table 0 02 002 values.
C*      Meters/sec are default units for 0 02 002.
C*      Bit 1 set = certified instruments.  Bit 2 - units are knots.
C*      Bit 3 - units are km/hr.  All 4 bits set - missing value.
C
	IF  ( isws .eq. 0 )  THEN
C
C*      Units are m/s but estimated (non-certified instruments), so no
C*      bits are set. 
C
	    UT_IWBF = 0.
	ELSE IF  ( isws .eq. 1 )  THEN
C
C*      Units are m/s with certified instruments.  Set bit 1.
C
	    UT_IWBF = PKFTBV ( 4, 1 )
	ELSE IF  ( isws .eq. 3 )  THEN
C
C*      Units are kts but estimated.  Set bit 2.
C
	    UT_IWBF = PKFTBV ( 4, 2 )
	ELSE IF  ( isws .eq. 4 )  THEN
C
C*      Units are kts with certified instruments.  Set bits 1 and 2.
C
	    UT_IWBF = PKFTBV ( 4, 1 ) + PKFTBV ( 4, 2 )
	ELSE 
	    UT_IWBF = R8BFMS
	END IF
C*
	RETURN
	END
