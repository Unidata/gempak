	SUBROUTINE DM_AFRE  ( iflno, nwords, locblk, iret )
C************************************************************************
C* DM_AFRE								*
C*									*
C* This subroutine adds a free block to the free block list.		*
C*									*
C* DM_AFRE  ( IFLNO, NWORDS, LOCBLK, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NWORDS		INTEGER		Number of words			*
C*	LOCBLK		INTEGER		Location of space		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	11/87	Fixed merge of new block with 2 blocks	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	length = nwords
	istart = locblk
	istop  = locblk + length
	iret   = 0
C
C*	Check to see if these words are contiguous with other blocks.
C
	i1 = 0
	i2 = 0
	DO  i = 1, knfree ( iflno )
C
C*	    Check whether new space ends at beginning of old space.
C
	    IF  ( istop .eq. kfreew ( 2, i, iflno ) )  THEN
		i1 = i
		length = length + kfreew ( 1, i, iflno ) 
	    END IF
	END DO
C
	DO  i = 1, knfree ( iflno )
C
C*	    Check whether new space starts at end of old space.
C
	    IF ( (kfreew (2,i,iflno ) + kfreew (1,i,iflno) )
     +						.eq. istart )  THEN
		i2     = i
		istart = kfreew ( 2, i, iflno )
		length = length + kfreew ( 1, i, iflno )
	    END IF
	END DO
C
C*	Eliminate space that was contiguous to current space.
C*	Be sure to eliminate later block first, so number of other
C*	block will not change.
C
	IF  ( i1 .lt. i2 )  THEN
	    CALL DM_EFRE  ( iflno, i2, ier )
	    IF  ( i1 .ne. 0 )  CALL DM_EFRE  ( iflno, i1, ier )
	  ELSE IF  ( i2 .lt. i1 )  THEN
	    CALL DM_EFRE  ( iflno, i1, ier )
	    IF  ( i2 .ne. 0 )  CALL DM_EFRE  ( iflno, i2, ier )
	END IF
C
C*	Find position in list for this space.
C
	loc = 0
	ip  = 1
	DO WHILE  ( ( loc .eq. 0 ) .and. ( ip .le. knfree ( iflno ) ) )
C
C*	    Check if number of words is greater than this free size.
C
	    IF  ( ( length .gt. kfreew ( 1, ip, iflno ) ) )  THEN
		loc = ip
	      ELSE
		ip  = ip + 1
	    END IF
	END DO
C
C*	If position was not found, put at the end of the list.
C*	Exit if this space will not fit in list.
C
	IF  ( loc .eq. 0 )  loc = knfree ( iflno ) + 1
	IF  ( loc .gt. kmfree ( iflno ) ) RETURN
C
C*	Move free space list to allow new blocks to be inserted.
C
	imove = knfree ( iflno ) + 1
	IF  ( imove .gt. kmfree ( iflno ) ) imove = kmfree ( iflno )
	DO  i = imove, loc + 1, -1
	    kfreew ( 1, i, iflno ) = kfreew ( 1, i-1, iflno )
	    kfreew ( 2, i, iflno ) = kfreew ( 2, i-1, iflno )
	END DO
C
C*	Add free space
C
	kfreew ( 1, loc, iflno ) = length
	kfreew ( 2, loc, iflno ) = istart
C
C*	Update number of free blocks.
C
	knfree ( iflno ) = imove
C*
	RETURN
	END
