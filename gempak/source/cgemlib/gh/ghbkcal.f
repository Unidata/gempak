       SUBROUTINE GH_BKCAL ( iseqcal, iret )
C************************************************************************
C* GH_BKCAL								*
C*									*
C* This subroutine looks up the sequence number of the last breakpoint  *
C* in California.							*
C*									*
C* GH_BKCAL ( ISEQCAL, IRET )                                  *
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	ISEQCAL 	INTEGER		Sequence number of last CA bkpt *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* S. Gilbert/NCEP    	10/07		Created				*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		iseqcal
C*
C-----------------------------------------------------------------------
	iret     = 0
	iseqcal  = iseqca
C*
	RETURN
	END
