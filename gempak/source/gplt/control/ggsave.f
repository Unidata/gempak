	SUBROUTINE GGSAVE  ( filnam, iframe, nframe, iret )
C************************************************************************
C* GGSAVE								*
C* 									*
C* This subroutine exports the current X window into an output file	*
C* specified by the file extension in filnam. If the window does not	*
C*  exist, an error is returned.					*
C*									*
C* GGSAVE  ( FILNAM, IFRAME, NFRAME, IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Gif Filename			*
C*	IFRAME		INT		Frame index number		*
C*	NFRAME		INT		Number of frames		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Bailey/HPC	 1/05						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVWIN.CMN'
C*
	CHARACTER*(*)	filnam
	INTEGER		iframe, nframe
C------------------------------------------------------------------------
	IF  ( ddev .ne. ' ' )  THEN
C
C*	    Close the plot file.
C
	    CALL DGSAVE  ( filnam, iframe, nframe, iret )
	END IF
C*
	CALL GEPLOT ( ier )
C*
	RETURN
	END
