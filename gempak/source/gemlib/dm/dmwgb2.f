	SUBROUTINE DM_WGB2 ( iflno, irow, icol, part, idthdr, idata, 
     +			     nword, iuscal, rmsval, iscan_mode, iret )
C************************************************************************
C* DM_WGB2								*
C*									*
C* This subroutine writes GRIB2 data to a DM file.			*
C*									*
C* DM_WDTI  ( IFLNO, IROW, ICOL, PART, IDTHDR, IDATA, NWORD, IUSCAL,	*
C*		RMSVAL, ISCAN_MODE, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	PART		CHAR*4		Part name			*
C*	IDTHDR (*)	INTEGER		Data header			*
C*	IDATA (NWORD)	INTEGER		Data				*
C*	NWORD		INTEGER		Length of data array		*
C*	IUSCAL		INTEGER		User defined scaling		*
C*	RMSVAL		REAL		User defined missing value	*
C*	ISCAN_MODE	INTEGER		Grid scanning mode flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -9 = invalid row or column	*
C*					-10 = invalid part name		*
C*					-13 = no write access		*
C*					-21 = incorrect data type	*
C**									*
C* Log:									*
C* S. Chiswell	Unidata/UCAR	 7/05	Created from DM_WDTI		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	part
	INTEGER		idata (*), idthdr (*)
	INTEGER		iarray(4), kx, ky, iscan_mode
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that file was opened for write access.
C
	IF  ( .not. wflag (iflno) ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Check for valid length.
C
	IF  ( nword .le. 0 )  RETURN
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
	IF  ( ( ktyprt ( iprt, iflno ) .ne. MDINTG ) .and.
     +	      ( ktyprt ( iprt, iflno ) .ne. MDGRID ) )  THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Get length of data header.
C
	ilenhd = klnhdr ( iprt, iflno )
C
C*	Check to see if data has already been written.
C*	First get pointer to data.
C
	ipoint = kpdata (iflno) + (irow-1) * kcol (iflno) * kprt (iflno)
     +		 + (icol-1) * kprt (iflno) + (iprt-1)
	CALL DM_RINT  ( iflno, ipoint, 1, istart, iret )
C
C*	Read length of previous data.
C
	IF ( istart .ne. 0 )  THEN
	    CALL DM_RINT  ( iflno, istart, 1, lenold, iret )
	  ELSE
	    lenold = 0
	END IF
C
C*	Get parameters for grid packing.
C*	Will add ipktyp + iuscal + kx + ky + iscan_mode + rmsval
C
	nadd = 6
	lennew = nword + ilenhd + nadd
C
C*	If old space is correct length, use it.
C
	IF  ( lenold .ne. lennew )  THEN
C
C*	    Otherwise, free old space.
C
	    IF  ( lenold .gt. 0 ) THEN
		length = lenold + 1
		CALL DM_AFRE ( iflno, length, istart, ier )
	    END IF
C
C*	    Get space for current data.
C
	    length = lennew + 1
	    CALL DM_GSPC  ( iflno, length, istart, iret )
	    IF  ( iret .ne. 0 )  RETURN
C
C*	    Write length to file.
C
	    CALL DM_WINT  ( iflno, istart, 1, lennew, iret )
	    IF  ( iret .ne. 0 )  RETURN
C*
	END IF
C
	kx = 0
	ky = 0
C
C*	Write header to file.
C
	isword = istart + 1
	IF  ( ilenhd .gt. 0 )  THEN
	    kx = idthdr(1)
	    ky = idthdr(2)
	    CALL DM_WINT  ( iflno, isword, ilenhd, idthdr, iret )
	    IF  ( iret .ne. 0 )  RETURN
	    isword = isword + ilenhd
	END IF
C
C*	Write data to file.
C
	kword = nword + nadd
	ipktyp = MDGRB2
	CALL DM_WINT  ( iflno, isword, 1, ipktyp, iret )
	iiword = isword + 1
        lendat = kword - 1

	iarray(1)=iuscal
	iarray(2)=kx
	iarray(3)=ky
	iarray(4)=iscan_mode
	CALL DM_WINT  ( iflno, iiword, 4, iarray, iret )
        IF  ( iret .ne. 0 )  RETURN
        iiword = iiword + 4
        lendat = lendat - 4
C
        CALL DM_WFLT  ( iflno, iiword, 1, rmsval, iret )
        IF  ( iret .ne. 0 )  RETURN
        iiword = iiword + 1
        lendat = lendat - 1
C
C*      Now write the data to the file.
C*      Set machine type to current machine so that byte data will not
C*      be translated.
C
        mmsave = kmachn ( iflno )
        kmachn ( iflno ) = MTMACH

        CALL DM_WINT  ( iflno, iiword, lendat, idata, iret )

	kmachn ( iflno ) = mmsave

	IF  ( iret .ne. 0 )  RETURN
C
C*	Write pointer to file.
C
	CALL DM_WINT  ( iflno, ipoint, 1, istart, iret )
C*
	RETURN
	END
