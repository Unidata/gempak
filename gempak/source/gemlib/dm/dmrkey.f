	SUBROUTINE DM_RKEY ( iflno, iret )
C************************************************************************
C* DM_RKEY								*
C*									*
C* This subroutine reads all the row and column key words in a 		*
C* DM file.  The keys are all stored in common.				*
C*									*
C* DM_RKEY  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Read row keys and convert to character.
C
	num = krkeys (iflno)
	CALL DM_RCH4 ( iflno, kprkey (iflno), num, kkrow (1,iflno), 
     +								iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Read col keys and convert to character.
C
	num = kckeys (iflno)
	CALL DM_RCH4 ( iflno, kpckey (iflno), num, kkcol (1,iflno),
     +								iret )
	IF  ( iret .ne. 0 ) RETURN
C*
	RETURN
	END
