	SUBROUTINE GSATIM  ( filnam, iret )
C************************************************************************
C* GSATIM								*
C*									*
C* This subroutine displays a satellite image.            		*
C*									*
C* GSATIM  ( FILNAM, IRET )						*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Krueger/EAI	 2/94						*
C* S. Jacobs/NMC	 3/94	Clean up				*
C* L. Williams/EAi       3/94	Removed blank comments from header,     *
C*                              changed file description and value of   *
C*                              isend.                                  *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*) 	filnam
C*
	CHARACTER	file*132
	INTEGER 	isend (35)
C-----------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C*	Use only the first 132 characters (33 words) of the file name.
C
	nwrd  = 33
	nchar = 132
	file  = filnam
C
	isend (1) = nwrd + 2
	isend (2) = FSATIM
C
	CALL ST_STOI  ( file, nchar, nv, isend (3), iret )
	CALL GPUT ( isend, 35, iret )
C
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
