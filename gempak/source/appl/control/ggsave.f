	SUBROUTINE GGSAVE  ( filnam, iframe, nframe, iret )
C************************************************************************
C* GGSAVE								*
C* 									*
C* This subroutine exports the current X window into an output file 	*
C* specified by the file extension in filnam. If the window does not	*
C*  exist, an error is returned.					*
C*									*
C* GGSAVE  ( FILNAM, IFRAME, NFRAME, IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		gif file name			*
C*	IFRAME		INT		Frame index number		*
C*	NFRAME		INT		Number of frames		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Bailey/HPC	 1/05						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
	INTEGER		iframe, nframe
C*
	CHARACTER	flnm*72
	INTEGER		isend (22)
C------------------------------------------------------------------------
C*	Store text string into integer array.
C
	flnm = filnam (1:72)
	CALL ST_STOI ( flnm, 72, nv, isend (5), ier )
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 22
	isend (2) = FGSAVE
C*
	isend (3) = iframe
	isend (4) = nframe
C*
	CALL GPUT  ( isend, 22, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C*
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
