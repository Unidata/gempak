	SUBROUTINE DM_WKEY ( iflno, iret )
C************************************************************************
C* DM_WKEY								*
C*									*
C* This subroutine writes the row and column key words to a file.  	*
C* The keys are all stored in common.					*
C*									*
C* DM_WKEY  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Move row key names into integer array and write to file.
C
	num = krkeys (iflno)
	CALL DM_WCH4  ( iflno, kprkey (iflno), num, kkrow (1,iflno),
     +			iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Move col key names into integer array and write to file.
C
	num = kckeys (iflno)
	CALL DM_WCH4  ( iflno, kpckey (iflno), num, kkcol (1,iflno),
     +			iret )
	IF  ( iret .ne. 0 ) RETURN
C*
	RETURN
	END
