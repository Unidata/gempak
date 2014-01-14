	SUBROUTINE DM_RDTI  ( iflno, irow, icol, part, idthdr, idata, 
     +			      nword, iret )
C************************************************************************
C* DM_RDTI								*
C*									*
C* This subroutine reads integer data from a DM file.			*
C*									*
C* DM_RDTI  ( IFLNO, IROW, ICOL, PART, IDTHDR, IDATA, NWORD, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	PART		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	IDTHDR (*)	INTEGER		Data header			*
C*	IDATA (NWORD)	INTEGER		Data				*
C*	NWORD		INTEGER		Length of data array		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					 -9 = invalid location		*
C*					-10 = invalid part name		*
C*					-15 = data not available	*
C*					-21 = incorrect data type	*
C*					-34 = incorrect record length	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* K. Tyle/GSC		 1/97	Check for excessive record length	*
C* S. Jacobs/NCEP	 8/13	Call DA lib for non-gempak files	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	part
	INTEGER		idata  (*)
	INTEGER		idthdr (*)
C------------------------------------------------------------------------
	nword = 0
C
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check for non-standard file.
C
	IF  ( .not. stdgem(iflno) )  THEN
	    CALL DA_RDTI ( iflno, irow, icol, part,
     +			   idthdr, idata, nword, iret )
     	    RETURN
	END IF
C
C*	Check for valid row and column positions.
C
	IF  ( ( irow .lt. 1 ) .or. ( irow .gt. krow (iflno) ) .or.
     +	      ( icol .lt. 1 ) .or. ( icol .gt. kcol (iflno) ) ) THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Get part number.
C
	iprt = 0
	DO  i = 1, kprt ( iflno )
	    IF  ( kprtnm ( i, iflno ) .eq. part ) iprt = i
	END DO
	IF  ( iprt .eq. 0 )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Check for valid data type.
C
	IF  ( ktyprt ( iprt, iflno ) .ne. MDINTG )  THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Get length of data header.
C
	ilenhd = klnhdr ( iprt, iflno )
C
C*	Get pointer to data.
C
	ipoint = kpdata (iflno) + (irow-1) * kcol (iflno) * kprt (iflno)
     +		 + (icol-1) * kprt (iflno) + (iprt-1)
	CALL DM_RINT  ( iflno, ipoint, 1, istart, iret )
C
C*	Read from file.
C
	IF ( istart .ne. 0 ) THEN
C
C*	    Read the first word which is the length.
C
	    CALL DM_RINT  ( iflno, istart, 1, length, iret )
	    isword = istart + 1
C
C*	    Check that length includes header and data.
C
	    IF  ( length .le. ilenhd )  THEN
		iret = -15
	      ELSE IF ( ABS(length) .gt. 1000000 ) THEN
C
C*	      Check for an unrealistically large record length,
C*	      which is indicative of an earlier write error.
C
		iret = -34
	      ELSE
C
C*		Read header.
C
		CALL DM_RINT  ( iflno, isword, ilenhd, idthdr, iret )
		IF  ( iret .ne. 0 )  RETURN
C
C*		Read data.
C
		nword  = length - ilenhd
		isword = isword + ilenhd
		CALL DM_RINT  ( iflno, isword, nword, idata, iret )
		IF  ( iret .ne. 0 )  nword = 0
	    END IF
	  ELSE
	    iret = -15
	END IF
C*
	RETURN
	END
