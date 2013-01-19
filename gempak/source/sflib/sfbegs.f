	SUBROUTINE SF_BEGS  ( isffln, iret )
C************************************************************************
C* SF_BEGS								*
C*									*
C* This subroutine resets the search pointers to the beginning of a 	*
C* surface file.  It does not change the time set by SF_STIM or the	*
C* station set by SF_TSTN.						*
C*									*
C* SF_BEGS  ( ISFFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	   0 = normal return		*
C*				  	  -3 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Reset pointer in DM file.
C
	CALL DM_BEGS  ( isffln, ier )
C
C*	Reset current row and column.
C
	krow (isffln) = 0
	kcol (isffln) = 0
C*
	RETURN
	END
