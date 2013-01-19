	SUBROUTINE HSMARK  ( imark, imkhw, size, iwidth, iret )
C************************************************************************
C* HSMARK - GN								*
C*									*
C* This subroutine sets the marker attributes.				*
C*									*
C* HSMARK  ( IMARK, IMKHW, SIZE, IWIDTH, IRET )				*
C*									*
C* Input parameters:							*
C*	IMARK		INTEGER		Marker number			*
C*	IMKHW		INTEGER		Marker hardware flag		*
C*	SIZE		REAL		Marker size			*
C*	IWIDTH		INTEGER		Marker line width		*
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
C*
	RETURN
	END
