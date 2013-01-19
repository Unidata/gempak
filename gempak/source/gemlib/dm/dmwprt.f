	SUBROUTINE DM_WPRT  ( iflno, iret )
C************************************************************************
C* DM_WPRT								*
C*									*
C* This subroutine writes general part information to a DM file.	*
C*									*
C* DM_WPRT  ( IFLNO, IRET )						*
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
C* M. desJardins/GSFC	 3/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iwrite = kppart ( iflno )
	nprt   = kprt   ( iflno )
C
C*	Move part names into integer array and write to file.
C
	CALL DM_WCH4  ( iflno, iwrite, nprt, kprtnm (1, iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iwrite = iwrite + nprt
C
C*	Write header lengths, data types and number of parameters.
C
	CALL DM_WINT  ( iflno, iwrite, nprt, klnhdr (1, iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iwrite = iwrite + nprt
C
	CALL DM_WINT  ( iflno, iwrite, nprt, ktyprt (1, iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iwrite = iwrite + nprt
C
	CALL DM_WINT  ( iflno, iwrite, nprt, kparms (1, iflno), iret )
	IF  ( iret .ne. 0 ) RETURN
	iwrite = iwrite + nprt
C
C*	Write parameter names.
C
	knt = 0
	DO  i = 1, nprt
	    DO  j = 1, kparms ( i, iflno )
		CALL DM_WCH4 ( iflno, iwrite, 1, kprmnm (j,i,iflno), 
     +							iret )
		iwrite = iwrite + 1
		IF  ( iret .ne. 0 )  RETURN
	    END DO
	END DO
C
C*	Write scale, offset and nbits to file.
C
	knt = 0
	DO  i = 1, nprt
	    DO  j = 1, kparms ( i, iflno )
		knt = knt + 1
		intarr (knt) = kscale ( j, i, iflno )
	    END DO
	END DO
	CALL DM_WINT ( iflno, iwrite, knt, intarr, iret )
	IF  ( iret .ne. 0 )  RETURN
	iwrite = iwrite + knt
C
	knt = 0
	DO  i = 1, nprt
	    DO  j = 1, kparms ( i, iflno )
		knt = knt + 1
		intarr (knt) = koffst ( j, i, iflno )
	    END DO
	END DO
	CALL DM_WINT ( iflno, iwrite, knt, intarr, iret )
	IF  ( iret .ne. 0 )  RETURN
	iwrite = iwrite + knt
C
	knt = 0
	DO  i = 1, nprt
	    DO  j = 1, kparms ( i, iflno )
		knt = knt + 1
		intarr (knt) = kbits  ( j, i, iflno )
	    END DO
	END DO
	CALL DM_WINT ( iflno, iwrite, knt, intarr, iret )
C*
	RETURN
	END
