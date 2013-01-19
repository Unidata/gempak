	SUBROUTINE DMESG  ( messag, iret )
C************************************************************************
C* DMESG								*
C* 									*
C* This subroutine will send a message to be handled by the device	*
C* driver.								*
C* 									*
C* DMESG  ( MESSAG, IRET )						*
C* 									*
C* Input parameters:							*
C* 									*
C*	MESSAG		CHAR*		Message				*
C*									*
C* Output parameters:							*
C*									*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/EAI	11/92						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	messag
	INTEGER		imesg (128), isend (2)
C------------------------------------------------------------------------
C*      Find the length of the input string in characters and word.
C
	lenc = 64
	lenw = 16
C
C*	Load input parameters into the buffers then write them to
C*	the mailbox.
C
	isend (1) = lenw + 2
	isend (2) = CMESG
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*      Convert the character string to an integer array and send 
C
	CALL ST_STOI  ( messag, lenc, nv, imesg, iret )
	CALL GPUT  ( imesg, lenw, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
