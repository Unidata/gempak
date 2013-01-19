	SUBROUTINE DM_CNST  ( iflno, icnst, istart, length, iret )
C************************************************************************
C* DM_CNST								*
C*									*
C* This subroutine writes a constant integer value to a DM file.	*
C*									*
C* DM_CNST  ( IFLNO, ICNST, ISTART, LENGTH, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ICNST		INTEGER		Constant value to write		*
C*	ISTART		INTEGER		First word to write		*
C*	LENGTH		INTEGER		Number of constants to write	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	INTEGER		ival (MBLKSZ) 
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Fill array with constant.
C
	DO  i = 1, MBLKSZ
	    ival (i) = icnst
	END DO
C
C*	Write constants a block at a time.
C*		nword = total number of words written
C*		mword = number of words to write during loop
C
	isword = istart
	nword  = 0
	DO WHILE  ( nword .lt. length )
	    nword = nword + MBLKSZ
	    mword = MBLKSZ
	    IF  ( nword .gt. length ) mword = mword - (nword - length)
	    CALL DM_WINT ( iflno, isword, mword, ival, iret )
	    IF  ( iret .ne. 0 ) nword = length
	    isword = isword + mword
	END DO
C*
	RETURN
	END
