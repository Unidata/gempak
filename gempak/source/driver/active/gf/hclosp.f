	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - GF								*
C* 									*
C* This subroutine closes the plot file/window.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Piper/SAIC	02/08	New for GF				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL

C
C*  Close the GF window.
C
	CALL GFCLOSP ( ixsize, iysize, ncurwn, iret )
	iright = ixsize
	ibot   = iysize
C
	RETURN
	END
