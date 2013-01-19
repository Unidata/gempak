	SUBROUTINE DM_ESPC  ( nblk, iret )
C************************************************************************
C* DM_ESPC								*
C*									*
C* This subroutine eliminates a free block from the scratch list.	*
C*									*
C* DM_ESPC  ( NBLK, IRET )						*
C*									*
C* Input parameters:							*
C*	NBLK		INTEGER		Block to eliminate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* M. desJardins/GSFC	 6/88	Fixed scratch space			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Check that the block is in the proper range.
C
	IF ( ( nblk .le. 0 ) .or. ( nblk .gt. msspce ) ) RETURN
C
C*	Eliminate block by moving blocks down.
C
	DO  i = nblk + 1, msspce
	    kscrch ( 1, i-1 ) = kscrch ( 1, i )
	    kscrch ( 2, i-1 ) = kscrch ( 2, i )
	END DO
C
C*	Update number of free blocks.
C
	msspce = msspce - 1
C*
	RETURN
	END
