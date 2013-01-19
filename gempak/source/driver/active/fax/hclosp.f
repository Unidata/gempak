	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - FAX								*
C* 									*
C* This subroutine closes the plot file.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* E. Wehner/EAi	7/96	Adopted to call Raster routine		*
C* S. Maxwell/GSC       6/97    Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C
	CALL RCLOSP ( iret )
C*
	RETURN
	END
