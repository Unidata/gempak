	SUBROUTINE DM_DPSR  ( iflno, iret )
C************************************************************************
C* DM_DPSR								*
C*									*
C* This subroutine deletes the primary search for a DM file.		*
C*									*
C* DM_DPSR  ( IFLNO, IRET )						*
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
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Turn search flag off.
C
	srcflg ( iflno ) = .false.
	ksnrow ( 0, iflno ) = 0
	ksncol ( 0, iflno ) = 0
C*
	RETURN
	END
