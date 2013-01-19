	SUBROUTINE DM_EFRE  ( iflno, nblk, iret )
C************************************************************************
C* DM_EFRE								*
C*									*
C* This subroutine eliminates a free block from the free block list.	*
C*									*
C* DM_EFRE  ( IFLNO, NBLK, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NBLK		INTEGER		Block to eliminate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Check that the block is in the proper range.
C
	IF ( ( nblk .le. 0 ) .or. ( nblk .gt. knfree ( iflno ))) RETURN
C
C*	Eliminate block by moving blocks down.
C
	DO  i = nblk + 1, knfree ( iflno )
	    kfreew ( 1, i-1, iflno ) = kfreew ( 1, i, iflno )
	    kfreew ( 2, i-1, iflno ) = kfreew ( 2, i, iflno )
	END DO
C
C*	Decrement free block counter.
C
	knfree ( iflno ) = knfree ( iflno ) - 1
C*
	RETURN
	END
