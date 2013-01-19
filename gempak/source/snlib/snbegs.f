	SUBROUTINE SN_BEGS  ( isnfln, iret )
C************************************************************************
C* SN_BEGS								*
C*									*
C* This subroutine resets the search pointers to the beginning of a 	*
C* sounding file.  It does not reset the time set by SN_STIM or the	*
C* station set by SN_TSTN.						*
C*									*
C* SN_BEGS  ( ISNFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	   0 = normal return		*
C*				  	  -4 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Reset pointer in DM file.
C
	CALL DM_BEGS  ( isnfln, ier )
C
C*	Reset current row and column.
C
	krow ( isnfln ) = 0
	kcol ( isnfln ) = 0
C*
	RETURN
	END
