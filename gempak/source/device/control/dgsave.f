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
C*	IFRAME		INT		Frame Index			*
C*	NFRAME		INT		Number of frames		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Bailey/HPC	 1/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
	INTEGER		iframe, nframe
C------------------------------------------------------------------------
	iret = 0
C
C*      Call driver make gif image from current window.
C
	CALL HGSAVE  ( filnam, iframe, nframe, iret )
C*
	RETURN
	END
