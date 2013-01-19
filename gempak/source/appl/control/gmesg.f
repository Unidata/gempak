	SUBROUTINE GMESG  ( messag, iret )
C************************************************************************
C* GMESG								*
C* 									*
C* This subroutine sends a "short title" frame label to be used by the  *
C* metafile driver.  The maximum length of the label string is 64       *
C* characters.                                                          *
C* 									*
C* GMESG  ( MESSAG, IRET )						*
C* 									*
C* Input parameters:							*
C*	MESSAG		CHAR*		Frame label                     *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/EAI	11/92						*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	messag
	CHARACTER	mmm*64
	INTEGER		imesg (128), isend (2)
C------------------------------------------------------------------------
C*      Find the length of the input string in characters and word.
C
	mmm = messag
	lenc = 64
	lenw = 16
C
C*	Load input parameters into the buffers then write them to
C*	the mailbox.
C
	isend (1) = lenw + 2
	isend (2) = FMESG
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*      Convert the character string to an integer array and send 
C
	CALL ST_STOI  ( mmm, lenc, nv, imesg, iret )
	CALL GPUT  ( imesg, lenw, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
