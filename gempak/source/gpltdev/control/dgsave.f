	SUBROUTINE DGSAVE  ( filnam, iframe, nframe, iret )
C************************************************************************
C* DGSAVE								*
C* 									*
C* This subroutine exports the current X window into an output file	*
C* specified by the file extension in filnam. If the window does not	*
C* exist, an error is returned.						*
C* 									*
C* DGSAVE  ( FILNAM, IFRAME, NFRAME, IRET )				*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Gif File name			*
C*	IFRAME		INT		Frame index number		*
C*	NFRAME		INT		Number of frames		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Bailey/HPC	 1/05						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
	INTEGER		iframe, nframe
C*
	CHARACTER	flnm*72
	INTEGER		isend (22)
C------------------------------------------------------------------------
C*      Store text string into integer array.
C
	flnm = filnam (1:72)
	CALL ST_STOI  ( flnm, 72, nv, isend (5), iret )
C
C*	Load input parameters into the buffers then write them to
C*	the mailbox.
C
	isend (1) = 22
	isend (2) = CGSAVE
C
	isend (3) = iframe
	isend (4) = nframe
C
	CALL GPUT  ( isend, 22, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
            IF ( ierr .eq. NFNCCD ) THEN
                iret = NOCWIN
            ELSE
	        iret = ierr
            ENDIF
	    RETURN
	END IF
C
	RETURN
	END
