	SUBROUTINE HSWIND  ( iwnd, size, iwidth, itype, hdsiz, iret )
C************************************************************************
C* HSWIND - VG								*
C*									*
C* This subroutine sets the wind attributes.				*
C*									*
C* HSWIND  ( IWND, SIZE, IWIDTH, ITYPE, HDSIZ, IRET )			*
C*									*
C* Input parameters:							*
C*	IWND		INTEGER		Wind category			*
C*					  1 = Barbs			*
C*					  2 = Arrows			*
C*	SIZE		REAL		Wind size			*
C*	IWIDTH		INTEGER		Wind line width			*
C*	ITYPE		INTEGER		Wind type			*
C*	HDSIZ		REAL		Wind arrow head size		*
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
	CALL VSWIND ( iwnd, size, iwidth, itype, hdsiz, iret )
C*
	RETURN
	END
