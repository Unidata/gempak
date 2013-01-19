	SUBROUTINE DM_ASPC  ( nwords, locblk, iret )
C************************************************************************
C* DM_ASPC								*
C*									*
C* This subroutine adds a free block to the scratch list.		*
C*									*
C* DM_ASPC  ( NWORDS, LOCBLK, IRET )					*
C*									*
C* Input parameters:							*
C*	NWORDS		INTEGER		Number of words			*
C*	LOCBLK		INTEGER		Location of space		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	12/87	Correct making space contiguous		*
C* M. desJardins/GSFC	 6/88	Fixed scratch space			*
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
	DO  i = 1, msspce 
C
C*	    Check whether new space ends at beginning of old space.
C
	    IF  ( istop .eq. kscrch ( 2, i ) )  THEN
		i1 = i
		length = length + kscrch ( 1, i ) 
	    END IF
	END DO
C
	DO  i = 1, msspce
C
C*	    Check whether new space starts at end of old space.
C
	    IF ( (kscrch ( 2, i ) + kscrch ( 1, i ) )
     +						.eq. istart )  THEN
		i2     = i
		istart = kscrch ( 2, i )
		length = length + kscrch ( 1, i )
	    END IF
	END DO
C
C*	Eliminate space that was contiguous to current space.
C*	Be sure to eliminate later block first, so number of other
C*	block will not change.
C
	IF  ( i1 .lt. i2 )  THEN
	    CALL DM_ESPC  ( i2, ier )
	    IF  ( i1 .ne. 0 )  THEN
		CALL DM_ESPC  ( i1, ier )
	    END IF
	  ELSE IF  ( i2 .lt. i1 )  THEN
	    CALL DM_ESPC  ( i1, ier )
	    IF  ( i2 .ne. 0 )  THEN
		CALL DM_ESPC  ( i2, ier )
	    END IF
	END IF
C
C*	Find position in list for this space.
C
	loc = 0
	ip  = 1
	DO WHILE  ( ( loc .eq. 0 ) .and. ( ip .le. msspce ) )
C
C*	    Check if number of words is greater than this free size.
C
	    IF  ( ( length .gt. kscrch ( 1, ip ) ) )  THEN
		loc = ip
	      ELSE
		ip  = ip + 1
	    END IF
	END DO
C
C*	If position was not found, put at the end of the list.
C*	Exit if this space will not fit in list.
C
	IF  ( loc .eq. 0 )  loc = msspce + 1
	IF  ( loc .gt. MMSCPR ) RETURN
C
C*	Move free space list to allow new blocks to be inserted.
C
	imove = msspce + 1
	IF  ( imove .gt. MMSCPR ) imove = MMSCPR
	DO  i = imove, loc + 1, -1
	    kscrch ( 1, i ) = kscrch ( 1, i-1 )
	    kscrch ( 2, i ) = kscrch ( 2, i-1 )
	END DO
C
C*	Add free space
C
	kscrch ( 1, loc ) = length
	kscrch ( 2, loc ) = istart
C
C*	Update number of free blocks.
C
	msspce = imove
C*
	RETURN
	END
