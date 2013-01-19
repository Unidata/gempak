	SUBROUTINE DM_RPRT  ( iflno, iret )
C************************************************************************
C* DM_RPRT								*
C*									*
C* This subroutine reads general part information from a DM file.	*
C*									*
C* DM_RPRT  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					-16 = invalid packing terms	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iread  = kppart ( iflno )
	nprt   = kprt   ( iflno )
C
C*	Read part names from file and translate into character.
C
	CALL DM_RCH4  ( iflno, iread, nprt, kprtnm (1,iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iread = iread + nprt
C
C*	Write header lengths, data types and number of parameters.
C
	CALL DM_RINT  ( iflno, iread, nprt, klnhdr (1,iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iread = iread + nprt
C
	CALL DM_RINT  ( iflno, iread, nprt, ktyprt (1,iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iread = iread + nprt
C
	CALL DM_RINT  ( iflno, iread, nprt, kparms (1,iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iread = iread + nprt
C
C*	Read parameter names.
C
	DO  i = 1, nprt
	    num = kparms ( i, iflno )
	    CALL DM_RCH4 ( iflno, iread, num, kprmnm (1,i,iflno), iret )
	    IF  ( iret .ne. 0 ) RETURN
	    iread = iread + num
	END DO
C
C*	Read scale, offset and nbits from file.
C
	DO  i = 1, nprt
	    num = kparms ( i, iflno )
	    CALL DM_RINT ( iflno, iread, num, kscale (1,i,iflno), iret )
	    IF  ( iret .ne. 0 ) RETURN
	    iread = iread + num
	END DO
C
	DO  i = 1, nprt
	    num = kparms ( i, iflno )
	    CALL DM_RINT ( iflno, iread, num, koffst (1,i,iflno), iret )
	    IF  ( iret .ne. 0 ) RETURN
	    iread = iread + num
	END DO
C
	DO  i = 1, nprt
	    num = kparms ( i, iflno )
	    CALL DM_RINT ( iflno, iread, num, kbits (1,i,iflno), iret )
	    IF  ( iret .ne. 0 ) RETURN
	    iread = iread + num
	END DO
C
C*	Set packing terms, if necessary.
C
	DO  i = 1, nprt
	  IF  ( ktyprt ( i, iflno ) .eq. MDRPCK )  THEN
	    CALL DP_SETP ( kparms (i, iflno), kscale (1, i, iflno),
     +			   koffst (1, i, iflno), kbits (1, i, iflno),
     +			   kpkno  (i, iflno), kwordp (i, iflno), ier )
	    IF  ( ier .ne. 0 )  THEN
		iret = -16
		CALL ER_WMSG ( 'DM', iret, kprtnm (i, iflno), ier )
		RETURN
	    END IF
	  END IF
	END DO
C*
	RETURN
	END
