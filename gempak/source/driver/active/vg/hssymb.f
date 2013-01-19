	SUBROUTINE HSSYMB  ( isym, itype, size, iwidth, iret )
C************************************************************************
C* HSSYMB - VG								*
C*									*
C* This subroutine sets the symbol attributes.				*
C*									*
C* HSSYMB  ( ISYM, ITYPE, SIZE, IWIDTH, IRET )				*
C*									*
C* Input parameters:							*
C*	ISYM		INTEGER		Symbol category			*
C*					  1 = Weather symbols		*
C*					  2 = Cloud type symbols	*
C*					  3 = Icing symbols		*
C*					  4 = Pressure tendency symbols	*
C*					  5 = Past weather symbols	*
C*					  6 = Sky cover symbols		*
C*					  7 = Special symbols		*
C*					  8 = Turbulence symbols	*
C*	ITYPE		INTEGER		Symbol type (specific per sym )	*
C*	SIZE		REAL		Symbol size			*
C*	IWIDTH		INTEGER		Symbol line width		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL VSSYMB ( isym, itype, size, iwidth, iret )
C*
	RETURN
	END
