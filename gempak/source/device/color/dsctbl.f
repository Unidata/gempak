	SUBROUTINE DSCTBL  ( ctblnm, iret )
C************************************************************************
C* DSCTBL								*
C* 									*
C* This subroutine processes a color table name 		 	*
C* 									*
C* DSCTBL  ( CTBLNM, IRET )						*
C* 									*
C* Input parameters:							*
C* 									*
C*	CTBLNM		CHAR*		Color table name		*
C*									*
C* Output parameters:							*
C*									*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/EAI	11/92						*
C* A. Chang/EAI		12/93	Added call to HSCTBL			*
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	ctblnm
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL HSCTBL ( ctblnm, ier )	
C*
	RETURN
	END
