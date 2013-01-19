	SUBROUTINE GSSPLN  ( isltyp, islstr, isldir, slsiz, islwid,
     +			     iret )
C************************************************************************
C* GSSPLN								*
C* 									*
C* This subroutine sets special line attributes including the special	*
C* line type number, stroke multiplier, direction indicator, size, and	*
C* width multiplier.							*
C* 									*
C* GSSPLN  ( ISLTYP, ISLSTR, ISLDIR, SLSIZ, ISLWID, IRET )		*
C*									*
C* Input parameters:							*
C*	ISLTYP		INTEGER		Special line type number 	*
C*	ISLSTR		INTEGER		Special line stroke multiplier	*
C*					   0 = no change		*
C*	ISLDIR		INTEGER		Special line direction indicator*
C*					   1 = up or out		*
C*					   0 = no change		*
C*					  -1 = down or in		*
C*	SLSIZ		REAL		Special line size		*
C*					   0 = no change		*
C*	ISLWID		INTEGER		Special line width multiplier	*
C*					   0 = no change		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97	Copied from GSLINE			*
C* D. Keiser/GSC	 6/97	Fix direction variable bug		*
C* D.W.Plummer/NCEP	 7/97	Added spcl line 6: arrow w/ filled head	*
C* S. Jacobs/NCEP	 4/98	Removed upper limit on line types	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check if these are current requested characteristics.
C
	 IF  ( ( ( isltyp .eq. ksltyp ) .or. ( isltyp. lt. 1 ) ) .and.
     +	       ( ( islstr .eq. kslstr ) .or. ( islstr .le. 0 ) ) .and.
     +	         ( isldir .eq. ksldir ) .and.
     +	         (  slsiz .eq. rslsiz ) .and.
     +	       ( ( islwid .eq. kslwid ) .or. ( islwid .le. 0 ) ) ) THEN
C
C*	    Set requested parameters.
C
	  ELSE
	    IF ( isltyp .ge. 1  ) ksltyp = isltyp
	    IF ( islstr .gt. 0  ) kslstr = islstr
	    IF ( isldir .ne. 0  ) ksldir = ISIGN(1, isldir) 
	    IF (  slsiz .gt. 0. ) rslsiz = slsiz
	    IF ( islwid .gt. 0  ) kslwid = islwid
C
C*	    Send characteristics to device if not already set.
C
	    IF  ( ( ddev .ne. ' ' ) .and.
     +		  ( ( ksltyp .ne. lsltyp ) .or.
     +		    ( kslstr .ne. lslstr ) .or. 
     +		    ( ksldir .ne. lsldir ) .or. 
     +		    ( rslsiz .ne. sslsiz ) .or.
     +		    ( kslwid .ne. lslwid ) ) )  THEN
	        CALL DSSPLN  ( ksltyp, kslstr, ksldir, rslsiz, kslwid,
     +			       lsltyp, lslstr, lsldir, sslsiz, lslwid,
     +			       iret )
	    END IF
	END IF
C*
	RETURN
	END
