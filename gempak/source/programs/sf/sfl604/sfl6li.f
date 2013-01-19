	SUBROUTINE SFL6LI  ( nlun,  lun, stn, cmpflg, time, rdata, 
     +			     cdata, datflg, dayflg, iret )
C************************************************************************
C* SFL6LI								*
C*									*
C* This subroutine writes the data to the requested devices.		*
C*									*
C* SFL6LI  ( NLUN, LUN, STN, CMPFLG, TIME, RDATA, CDATA, DATFLG,	*
C*           DAYFLG, IRET )						*
C*									*
C* Input parameters:							*
C*	NLUN		INTEGER		Number of devices		*
C*	LUN  (NLUN)	INTEGER		LUNs of devices			*
C*	STN		CHAR*		Station id			*
C*	CMPFLG (*)	LOGICAL		Compute flags			*
C*	TIME		CHAR*		Date/time 			*
C*	RDATA (*)	REAL		Real data 			*
C*	CDATA (*)	CHAR*		Character data 			*
C*	DATFLG		LOGICAL		Missing data flag		*
C*	DAYFLG		LOGICAL		Change in station flag		*
C*									*
C* Ouput parameters:							*
C*	IRET		INTEGER		Return status			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84						*
C* I. Graffman/RDS	 7/86   Rounded integer problem fixed		*
C* M. desJardins/GSFC	10/86	Cleaned up				*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Fixed for new PT library		*
C* M. desJardins/GSFC	 6/88	Changed format for station numbers	*
C* S. Jacobs/EAI	10/92	Changed horizontal spacing		*
C* P. Bruehl/Unidata	 9/94	Changed numbering to match list, added  *
C*				P06I to precip field,fixed snow		*
C* K. Tyle/GSC		 1/97	Changed numbering to match list		*
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	SAVE		oldtim
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		lun (*)
	REAL		rdata (*)
	CHARACTER*(*)	stn, time, cdata (*)
	LOGICAL		cmpflg (*), datflg, dayflg
C*
	CHARACTER	outbuf*79, buf2*48, oldtim*7
	INCLUDE		'ERMISS.FNC'
	DATA		buf2/' '/, oldtim/' '/
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Check to see it the day is the same then move time into output
C*	buffer.
C
	outbuf = ' '
C*
	IF ( time ( 1 : 7 ) .ne. oldtim ) buf2 ( 1: ) = time ( 1 : 7 )
	
	outbuf ( 1 : 2 )  =  time ( 8 : 9 )
C
C*	Get length of station and right justify.
C
	CALL ST_LSTR  ( stn, lens, ier )
	IF  ( ( lens .gt. 6 ) .or. ( lens .eq. 0 ) ) lens = 6
	outbuf ( 9 - lens : 8 ) = stn ( 1 : lens )
C
C*	Check that there is data to be moved.
C
	IF  ( datflg )  THEN
C
C*	  Right justify the low, mid and high cloud information.
C
	  IF  ( cmpflg (1) .and. ( cdata (1) .ne. ' ' ) )  THEN
	    CALL ST_LSTR  ( cdata (1), isiz, ier )
	    IF  ( isiz .eq. 0 )  isiz = 1
	    outbuf ( 15 - isiz : 14 ) = cdata ( 1 ) ( 1 : isiz )
	  END IF
C*
	  IF  ( cmpflg (2) .and. ( cdata (2) .ne. ' ' ) )  THEN
	    CALL ST_LSTR  ( cdata (2), isiz, ier )
	    IF  ( isiz .eq. 0 )  isiz = 1
	    outbuf ( 21 - isiz : 20 ) = cdata ( 2 ) ( 1 : isiz )
	  END IF
C*
	  IF  ( cmpflg (3) .and. ( cdata (3) .ne. ' ' ) )  THEN
	    CALL ST_LSTR  ( cdata (3), isiz, ier )
	    IF  ( isiz .eq. 0 )  isiz = 1
	    outbuf ( 27 - isiz : 26 ) = cdata ( 3 ) ( 1 : isiz )
	  END IF
C
C*	  Add the visibility.
C
	  IF  ( cmpflg (4) .and. ( .not. ERMISS ( rdata (4) ) ) )  THEN
	    WRITE  ( outbuf ( 28 : ), 500, IOSTAT = ier )  rdata (4)
500	    FORMAT ( F4.1 )
	  END IF
C
C*	  Add the right justified weather code.
C
	  IF  ( cmpflg (5) .and. ( cdata (5) .ne. ' ' ) )  THEN
	    CALL ST_LSTR  ( cdata (5), isiz, ier )
	    IF  ( isiz .eq. 0 )  cdata (5) = ' '
	    IF  ( ( isiz .eq. 0 ) .or. ( isiz .gt. 7 ) ) isiz = 7
	    outbuf ( 38 - isiz : 37 ) = cdata ( 5 ) ( 1 : isiz )
	  END IF
C
C*	  Add PMSL.
C
	  IF  ( cmpflg ( 6 ) .and. ( .not. ERMISS ( rdata (6) ))) THEN
	    WRITE  ( outbuf ( 39 : ), 510, IOSTAT = ier )  rdata (6)
510	    FORMAT ( F6.1 )
	  END IF
C
C*	  Add the temperature and dewpoint temperature.
C
	  IF  ( cmpflg ( 7 ) .and. (.not. ERMISS ( rdata (7) ))) THEN
	    itmp = NINT  ( rdata (7) )
	    WRITE  ( outbuf ( 46 : ), 520, IOSTAT = ier ) itmp
520	    FORMAT ( I3 )
	  END IF
C*
	  IF  ( cmpflg ( 8 ) .and. ( .not. ERMISS ( rdata (8) ))) THEN
	    itmp = NINT (rdata (8))
	    WRITE  ( outbuf ( 50 : ), 520, IOSTAT = ier ) itmp
	  END IF
C
C*	  Add the direction and speed.
C
	  IF  ( cmpflg ( 9 ) .and. ( .not. ERMISS ( rdata (9) ))) THEN
	    itmp = NINT (rdata (9)/10.)
	    WRITE  ( outbuf ( 54 : ), 420, IOSTAT = ier ) itmp
420	    FORMAT ( I2 )
	  END IF
C*
	  IF  ( cmpflg (10) .and. ( .not. ERMISS ( rdata (10) ))) THEN
	    itmp = NINT (rdata (10))
	    WRITE  ( outbuf ( 56 : ), 420, IOSTAT = ier ) itmp
	  END IF
C
C*	Add gusts and altimeter.
C
	  IF  ( cmpflg (11) .and. ( .not. ERMISS ( rdata (11) )))  THEN
	    itmp = NINT (rdata (11))
	    WRITE  ( outbuf ( 59: ), 420, IOSTAT = ier )  itmp
	  END IF
C*
	  IF  ( cmpflg (12) .and. ( .not. ERMISS ( rdata (12) ))) THEN
	    WRITE  ( outbuf ( 62: ), 530, IOSTAT = ier )  rdata (12)
530	    FORMAT ( F5.2 )
	  END IF
C
C*	Add the pressure tendency.
C
	  IF  ( cmpflg (13) .and. ( .not. ERMISS ( rdata (13) )))  THEN
	    WRITE  ( outbuf ( 68: ), 540, IOSTAT = ier ) rdata (13)
540	    FORMAT ( F4.1 )
	  END IF
C
C*	Add the precipitation.
C
	  IF ( time ( 8 : 9 ) .eq. '12' )  THEN
	      IF  ( cmpflg (16) .and. ( .not. ERMISS ( rdata (16) )))
     +								THEN
		  IF ( rdata (16) .eq. 0 )  THEN
		      outbuf ( 75 : ) = 'T'
		    ELSE
C
C*		P24I reported at 12Z
C
	    	      WRITE  ( outbuf ( 73: ), 550, IOSTAT = ier )
     +							rdata (16)
550	    FORMAT ( F4.2 )
		  END IF
	      ENDIF
	    ELSE
	      IF  ( cmpflg (14) .and. ( .not. ERMISS ( rdata (14) )))
     +								THEN
		  IF ( rdata (14) .eq. 0 )  THEN
		      outbuf ( 75 : ) = 'T'
		    ELSE
C
C*		P03I reported 3hr precip
C
	    	      WRITE  ( outbuf ( 73: ), 550, IOSTAT = ier )
     +							rdata (14)
		  ENDIF
	      ELSE IF ( cmpflg (15) .and.
     +			 ( .not. ERMISS ( rdata (15) )))  THEN
		  IF ( rdata (15) .eq. 0 )  THEN
		      outbuf ( 75 : ) = 'T'
		    ELSE
C
C*		P06I reported 6hr precip
C
	    	      WRITE  ( outbuf ( 73: ), 550, IOSTAT = ier )
     +							rdata (15)
		  ENDIF
	      ENDIF
	  ENDIF
C
C*	Add the snow data.
C
	  IF  ( cmpflg (17) .and. ( .not. ERMISS ( rdata (17) )))  THEN
	    isnow = NINT ( rdata (17) )
	    WRITE  ( outbuf ( 78: ), 420, IOSTAT = ier ) isnow
	  END IF
C*
	END IF
C
C*	Write data to requested luns.
C
	DO  ilun = 1, nlun
	  IF ( ( time ( 1 : 7 ) .ne. oldtim ) .or. ( .not. dayflg ) )
     +				      WRITE ( lun ( ilun ), 5000 ) buf2
	  WRITE  ( lun ( ilun ), 5000 )  outbuf
5000	  FORMAT ( ' ', A )
	END DO
	oldtim = time ( 1 : 7 )
	dayflg = .true.
C*
	RETURN
	END
