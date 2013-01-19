	SUBROUTINE HQCLRS  ( ibank, ncolr, iret )
C************************************************************************
C* HQCLRS - GN								*
C* 									*
C* This subroutine gets the colors in a color bank.			*
C* 									*
C* HQCLRS  ( IBANK, NCOLR, IRET )					*
C* 									*
C* Input parameters:							*
C*	IBANK		INTEGER		Color bank			*
C*									*
C* Output parameters:							*
C*	NCOLR		INTEGER		Number of colors		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	11/95						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Piper/SAIC	1/02	Set output values			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	ncolr = 1
C
	iret = NORMAL
C
	RETURN
	END
