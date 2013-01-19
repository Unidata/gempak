	SUBROUTINE HSLINE  ( mltyp, mlthw, mlwid, mlwhw, iret )
C************************************************************************
C* HSLINE - VG								*
C*									*
C* This subroutine sets the line attributes.				*
C*									*
C* HSLINE  ( MLTYP, MLTHW, MLWID, MLWHW, IRET )				*
C*									*
C* Input parameters:							*
C*	MLTYP		INTEGER		Line type			*
C*	MLTHW		INTEGER		Sw/hw line type flag		*
C*	MLWID		INTEGER		Line width size multiplier	*
C*	MLWHW		INTEGER		Sw/hw line width flag		*
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
	CALL VSLINE  ( mltyp, mlthw, mlwid, mlwhw, iret )
C*
	RETURN
	END
