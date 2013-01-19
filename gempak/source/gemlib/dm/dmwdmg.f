	SUBROUTINE DM_WDMG ( iflno, iret )
C************************************************************************
C* DM_WDMG								*
C*									*
C* This subroutine writes the data management record to a DM file.	*
C*									*
C* DM_WDMG  ( IFLNO, IRET )						*
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
C
	INTEGER		iarray ( 4 + 2 * MMFREE )
C------------------------------------------------------------------------
C*	Get the pointer to the data management record.
C
	isword = kpdmgt ( iflno )
	length = kldmgt ( iflno )
C
C*	Put the record into an array.
C
	iarray ( 1 ) = kpnext ( iflno )
	iarray ( 2 ) = kmfree ( iflno )
	iarray ( 3 ) = knfree ( iflno )
	iarray ( 4 ) = klstwd ( iflno )
	next = 5
	DO  i = 1, kmfree ( iflno )
	    iarray ( next ) = kfreew ( 1, i, iflno )
	    iarray (next+1) = kfreew ( 2, i, iflno )
	    next = next + 2
	END DO
C
C*	Write the record.
C
	CALL DM_WINT ( iflno, isword, length, iarray, iret )
C*
	RETURN
	END
