	SUBROUTINE GSCTBL  ( ctblnm, iret )
C************************************************************************
C* GSCTBL								*
C* 									*
C* This subroutine will send a color table name to be handled           *
C* by the device driver.                                                *
C* 									*
C* GSCTBL  ( CTBLNM, IRET )						*
C*									*
C* Input parameters:							*
C* 	CTBLNM		CHAR*		Color table name		*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/EAI	11/92						*
C* A. Chang/EAI		12/93						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	ctblnm
C------------------------------------------------------------------------
	iret = NORMAL
C*
	CALL DSCTBL ( ctblnm, ier )
C*
	RETURN
	END
