	SUBROUTINE DM_BEGS  ( iflno, iret )
C************************************************************************
C* DM_BEGS								*
C*									*
C* This subroutine restarts a search at the beginning of a DM file.	*
C*									*
C* DM_BEGS  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Reset pointers to beginning of file.
C
	ksrow ( iflno ) = 0
	kscol ( iflno ) = 0
C*
	RETURN
	END
