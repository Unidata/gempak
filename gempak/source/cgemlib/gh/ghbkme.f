       SUBROUTINE GH_BKME ( jseqme, iret )
C************************************************************************
C* GH_BKME								*
C*									*
C* This subroutine looks up the sequence number of the last breakpoint  *
C* in Maine.							*
C*									*
C* GH_BKME ( JSEQME, IRET )                                  *
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	ISEQME 		INTEGER		Sequence number of last ME bkpt *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* S. Gilbert/NCEP    	06/09		Created				*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		jseqme
C*
C-----------------------------------------------------------------------
	iret     = 0
	jseqme  = iseqme
C*
	RETURN
	END
