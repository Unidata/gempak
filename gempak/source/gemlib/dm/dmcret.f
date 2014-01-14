	SUBROUTINE DM_CRET ( filnam, iftype, ifsrce, nfhdrs, fhdnam, 
     +	                     ifhlen, ifhtyp, nrow, nrkeys, keyrow, 
     +			     ncol, nckeys, keycol, nprt, prtnam, 
     +	                     lenhdr, ityprt, nparms, maxprm, prmnam, 
     +	                     iscale, ioffst, nbits, iflno, iret )
C************************************************************************
C* DM_CRET								*
C*									*
C* This subroutine creates a new DM file.  The arrays PRMNAM, ISCALE,	*
C* IOFFST, and NBITS must be two-dimensional arrays in the calling	*
C* program whose first dimension is MAXPRM.  After the file is		*
C* created, it is left open with write access.				*
C*									*
C* DM_CRET ( FILNAM, IFTYPE, IFSRCE, NFHDRS, FHDNAM, IFHLEN, IFHTYP,	*
C*           NROW,   NRKEYS, KEYROW, NCOL,   NCKEYS, KEYCOL, NPRT,   	*
C*           PRTNAM, LENHDR, ITYPRT, NPARMS, MAXPRM, PRMNAM, ISCALE, 	*
C*           IOFFST, NBITS,  IFLNO,  IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	IFTYPE		INTEGER		File type			*
C*	IFSRCE		INTEGER		File source			*
C*	NFHDRS		INTEGER		Number of file headers		*
C*	FHDNAM (NFHDRS)	CHAR*4		File header names		*
C*	IFHLEN (NFHDRS)	INTEGER		File header lengths		*
C*	IFHTYP (NFHDRS)	INTEGER		File header data types		*
C*	NROW		INTEGER		Number of rows			*
C*	NRKEYS		INTEGER		Number of row keys		*
C*	KEYROW (NRKEYS)	CHAR*4		Row key names			*
C*	NCOL		INTEGER		Number of columns		*
C*	NCKEYS		INTEGER		Number of column keys		*
C*	KEYCOL (NCKEYS)	CHAR*4		Column key names		*
C*	NPRT		INTEGER		Number of parts			*
C*	PRTNAM (NPRT)	CHAR*4		Part names			*
C*	LENHDR (NPRT)	INTEGER		Part header lengths		*
C*	ITYPRT (NPRT)	INTEGER		Part data types			*
C*	NPARMS (NPRT)	INTEGER		Number of parameters / part	*
C*	MAXPRM		INTEGER		Maximum number of parameters	*
C*	PRMNAM		CHAR*		Parameter names			*
C*	  (MAXPRM,NPRT)							*
C*	ISCALE		INTEGER		Scaling for packed real		*
C*	  (MAXPRM,NPRT)							*
C*	IOFFST		INTEGER		Offset for packed real		*
C*	  (MAXPRM,NPRT)							*
C*	NBITS		INTEGER		Number of bits for packed real	*
C*	  (MAXPRM,NPRT)							*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = file cannot be created	*
C*					 -3 = too many files open	*
C*					 -5 = invalid dimension sizes	*
C*					 -6 = write error		*
C*					-16 = invalid packing terms	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C* S. Jacobs/NCEP	 8/13	Added flag for standard gempak file	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	fhdnam (*), keyrow (*), keycol (*), prtnam (*),
     +			prmnam (MAXPRM, *), filnam
	INTEGER		ifhlen (*), ifhtyp (*), lenhdr (*), ityprt (*),
     +			nparms (*), iscale (MAXPRM,*), 
     +			ioffst (MAXPRM,*), nbits (MAXPRM,*)
C------------------------------------------------------------------------
C*	Get a file number for this file.  Return if there are no more
C*	file numbers.
C
	CALL DM_GETF  ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that sizes requested are positive and smaller than maximums.
C
	IF  ( ( nrow .lt. 1 ) .or. ( ncol .lt. 1 ) .or. 
     +	      ( (nrow + ncol) .gt. MMHDRS ) .or. 
     +	      ( nprt .lt. 1 ) .or. ( nprt .gt. MMPRT ) .or.
     +	      ( MAXPRM .gt. MMPARM )  .or.  ( nfhdrs .gt. MMFHDR ) .or. 
     +	      ( nrkeys .gt. MMKEY  )  .or.  ( nckeys .gt. MMKEY ) )
     +							THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Create a new file with record size MBLKSZ.  Print message if
C*	file cannot be created and return.
C
	CALL FL_DCRE ( filnam, MBLKSZ, lun, ier )
	IF  ( ier .ne. 0 ) THEN
	    CALL ER_WMSG  ( 'FL', ier, filnam, ier1 )
	    iret = -1
	    RETURN
	  ELSE
	    lundm  ( iflno ) = lun
	    wflag  ( iflno ) = .true.
	    kshare ( iflno ) = .false.
	    stdgem ( iflno ) = .true.
	END IF
C
C*	Save all data management values in common except first free word.
C
	kmfree (iflno) = MMFREE
	knfree (iflno) = 0
	DO  i = 1, MMFREE
	    kfreew ( 1, i, iflno ) = 0
	    kfreew ( 2, i, iflno ) = 0
	END DO
	kpdmgt (iflno) = MBLKSZ + 1
	kldmgt (iflno) = 4 + 2 * MMFREE
	klstwd (iflno) = 0
	nxtwrd = kpdmgt (iflno) + kldmgt (iflno)
C
C*	Save row and column header information in common.
C
	kversn (iflno) = 1
	kftype (iflno) = iftype
	kfsrce (iflno) = ifsrce
C
	krow   (iflno) = nrow
	krkeys (iflno) = nrkeys
	kcol   (iflno) = ncol
	kckeys (iflno) = nckeys
C
C*	Save row and column keys.
C
	DO  i = 1, nrkeys
	    kkrow (i,iflno) = keyrow (i)
	END DO
	DO  i = 1, nckeys
	    kkcol (i,iflno) = keycol (i)
	END DO
C
C*	Set pointers to row and column keys.
C
	CALL DM_WORD ( nxtwrd, irec, iword, iret )
	IF ( iword .ne. 1 ) nxtwrd = irec * MBLKSZ + 1
	kprkey (iflno) = nxtwrd
	kpckey (iflno) = kprkey (iflno) + nrkeys
	nxtwrd = kpckey (iflno) + nckeys
C
C*	Save file header information.  Save an extra word with each 
C*	file header to save actual length.
C
	kpfile (iflno) = nxtwrd
	lenfil = 0
	kfhdrs (iflno) = nfhdrs
	DO  i = 1, nfhdrs
	    lenfil = lenfil + ifhlen (i) + 1
	    kfhnam (i,iflno) = fhdnam (i)
	    kfhlen (i,iflno) = ifhlen (i)
	    kfhtyp (i,iflno) = ifhtyp (i)
	END DO
	nxtwrd = nxtwrd + 3 * nfhdrs + lenfil
C
C*	Set pointers to headers.  Start on a block boundary.
C
	CALL DM_WORD ( nxtwrd, irec, iword, iret )
	IF ( iword .ne. 1 ) nxtwrd = irec * MBLKSZ + 1
	kprowh (iflno) = nxtwrd
	kpcolh (iflno) = kprowh (iflno) + nrow * ( nrkeys + 1 )
	nxtwrd = kpcolh (iflno) + ncol * ( nckeys + 1 )
C
C*	Save part information.  Start on block boundary.
C
	kprt (iflno) = nprt
	CALL DM_WORD  ( nxtwrd, irec, iword, iret )
	IF  ( iword .ne. 1 )  nxtwrd = irec * MBLKSZ + 1
	kppart (iflno) = nxtwrd
	lenprt = 0
	DO  i = 1, nprt
	    kprtnm ( i, iflno ) = prtnam (i)
	    klnhdr ( i, iflno ) = lenhdr (i)
	    ktyprt ( i, iflno ) = ityprt (i)
	    kparms ( i, iflno ) = nparms (i)
	    lenprt = lenprt + nparms (i)
	    DO  j = 1, nparms (i)
		kprmnm ( j, i, iflno ) = prmnam ( j, i )
		kscale ( j, i, iflno ) = iscale ( j, i )
		koffst ( j, i, iflno ) = ioffst ( j, i )
		kbits  ( j, i, iflno ) = nbits  ( j, i )
	    END DO
C
C*	    Set packing terms, if necessary.
C
	    IF  ( ktyprt ( i, iflno ) .eq. MDRPCK )  THEN
		CALL DP_SETP ( kparms (i, iflno), kscale (1, i, iflno),
     +			     koffst (1, i, iflno), kbits (1, i, iflno),
     +			     kpkno  (i, iflno), kwordp (i, iflno), ier )
		IF  ( ier .ne. 0 )  THEN
		    iret = -16
		    CALL ER_WMSG ( 'DM', iret, kprtnm (i, iflno), ier )
		    GO TO 900
		END IF
	    END IF
	END DO
	nxtwrd = kppart ( iflno ) + 4 * nprt + 4 * lenprt
C
C*	Set pointer to data pointers.  Start on block boundary.
C
	CALL DM_WORD ( nxtwrd, irec, iword, iret )
	IF ( iword .ne. 1 ) nxtwrd = irec * MBLKSZ + 1
	kpdata (iflno) = nxtwrd
C
C*	Save missing data values for file label.
C
	kmachn ( iflno ) = MTMACH
	kmissd ( iflno ) = IMISSD
	smissd ( iflno ) = RMISSD
C
C*	This completes the file label.  Write to file.
C
	CALL DM_WLBL ( iflno, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Find first free word for data which is aligned on block.  
C*	Save data management record.
C
	nxtwrd = kpdata (iflno) + nprt * nrow * ncol
	CALL DM_WORD ( nxtwrd, irec, iword, iret )
	IF ( iword .ne. 1 ) nxtwrd = irec * MBLKSZ + 1
	kpnext ( iflno ) = nxtwrd
	CALL DM_WDMG ( iflno, iret )
	IF ( iret .ne. 0 ) GO TO 900
C
C*	Write the row and column keys to the file.
C
	CALL DM_WKEY ( iflno, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Initialize file headers to zero.
C*	Write zeros for the file header information.
C
	CALL DM_WFIL ( iflno, iret )
	IF  ( iret .ne. 0 ) GO TO 900
	kpf = kpfile (iflno) + 3 * nfhdrs
	CALL DM_CNST ( iflno, 0, kpf, lenfil, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Write the part information.
C
	CALL DM_WPRT ( iflno, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Initialize all headers values to a missing data value.
C*	KHEADR ( 0, ... ) is used as a flag to indicate no header
C*	has been defined.
C
	knt = 0
	DO  i = 1, nrow
	    knt = knt + 1
	    DO  j = 0, nrkeys
		kheadr ( j, knt, iflno ) = IMISSD
	    END DO
	END DO
C
	DO  i = 1, ncol
	    knt = knt + 1
	    DO  j = 0, nckeys
		kheadr ( j, knt, iflno) = IMISSD
	    END DO
	END DO
C
C*	Set flags in common to say that no headers have been defined.
C
	klstcl ( iflno ) = 0
	klstrw ( iflno ) = 0
C
C*	Write headers to file.
C
	CALL DM_WHDA ( iflno, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Initialize data pointers to zero and write to file.
C
	length = nrow * ncol * nprt
	CALL DM_CNST ( iflno, 0, kpdata (iflno), length, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Flush write buffers.
C
	CALL DM_FWRT  ( iflno, iret )
	IF  ( iret .ne. 0 ) GO TO 900
C
C*	Set search information in common.
C
	srcflg ( iflno ) = .false.
	nsrch  ( iflno ) =  0
	ksrow  ( iflno ) =  0
	kscol  ( iflno ) =  0
C
	GO TO 999
C
900	CONTINUE
C
C*	Close file if an error was encountered.
C
	CALL DM_CLOS ( iflno, ier )
C*
999	CONTINUE
	RETURN
	END
