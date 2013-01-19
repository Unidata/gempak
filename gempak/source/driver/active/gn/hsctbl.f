	SUBROUTINE HSCTBL  ( ctblnm, iret )
C************************************************************************
C* HSCTBL - GN								*
C* 									*
C* This subroutine processes a color table name. 		 	*
C* 									*
C* HSCTBL  ( CTBLNM, IRET )						*
C* 									*
C* Input parameters:							*
C*	CTBLNM		CHAR*		Color table name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	ctblnm
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
