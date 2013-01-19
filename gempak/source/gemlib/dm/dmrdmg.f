	SUBROUTINE DM_RDMG ( iflno, iret )
C************************************************************************
C* DM_RDMG								*
C*									*
C* This subroutine reads the data management record from a DM file.	*
C*									*
C* DM_RDMG  ( IFLNO, IRET )						*
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
C* M. desJardins/GSFC	12/87	Added last word in shared files		*
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
C*	Read the record into an array.
C
	CALL DM_RINT  ( iflno, isword, length, iarray, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Put the record into the common area.
C
	kpnext ( iflno ) = iarray ( 1 )
	kmfree ( iflno ) = iarray ( 2 ) 
	knfree ( iflno ) = iarray ( 3 )
	klstwd ( iflno ) = iarray ( 4 )
	next = 5
	DO  i = 1, kmfree ( iflno )
	    kfreew ( 1, i, iflno ) = iarray ( next ) 
	    kfreew ( 2, i, iflno ) = iarray (next+1) 
	    next = next + 2
	END DO
C*
	RETURN
	END
