	SUBROUTINE MT_CNCLTP ( stcltp, lencltp, iscloud, iret )
C************************************************************************
C* MT_CLTP							        *
C*								        *
C* This subroutine will decode low, middle, and/or high cloud types	*
C* from the remarks section of a METAR report. (Canadian Stations)	*
C* The cloud type values are stored in common.		        	*
C*								        *
C* MT_CNCLTP ( STCLTP, LENCLTP, ISCLOUD, IRET )				       	        *
C*								        *
C* Input parameters:						        *
C*	STCLTP		CHAR*		Possible cloud string		*
C*      LENCLTP         INTEGER         Length of cloud string          *
C*	ISCLOUD		LOGICAL		
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRCTYL)	REAL		Low-level cloud type WMO 0513   *
C*	RIVALS(IRCTYM)	REAL		Mid-level cloud type WMO 0515   *
C*	RIVALS(IRCTYH)	REAL		High-level cloud type WMO 0509  *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return 	        *
C*					 14 = decode error 	        *
C**								        *
C* Log:							       	  	*
C* C. Page/UQAM		8/01	Original author			        *
C* S. Chiswell/Unidata	8/01	cleanup				        *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stcltp
        INTEGER         lencltp
	LOGICAL		iscloud
C-----------------------------------------------------------------------
	CHARACTER*1     chr
	INTEGER		low, middle, high, lowt, middlet, hight, pos
	INTEGER		highc, middlec, lowc

	iret = 0

	IF ( iscloud ) RETURN
C
C*	Assume remark is 3-12 characters
C
	IF ( stcltp ( 1:3 ) .ne. 'CIG' .and.
     +		stcltp ( 1:3 ) .ne. 'SFC' .and.
     +		stcltp ( 1:3 ) .ne. 'CLR' .and.
     +		stcltp ( 1:3 ) .ne. 'OVC' .and.
     +		stcltp ( 1:3 ) .ne. 'ASO' .and.
     +		stcltp ( 1:3 ) .ne. 'SCT' ) THEN

	   IF ( stcltp ( 1:2 ) .eq. 'AC' .or.
     +		stcltp ( 1:3 ) .eq. 'ACC' .or.
     +		stcltp ( 1:2 ) .eq. 'AS' .or.
     +		stcltp ( 1:2 ) .eq. 'FG' .or.
     +		stcltp ( 1:2 ) .eq. 'CB' .or.
     +		stcltp ( 1:2 ) .eq. 'CC' .or.
     +		stcltp ( 1:2 ) .eq. 'CF' .or.
     +		stcltp ( 1:2 ) .eq. 'CI' .or.
     +		stcltp ( 1:2 ) .eq. 'CS' .or.
     +		stcltp ( 1:2 ) .eq. 'CU' .or.
     +		stcltp ( 1:2 ) .eq. 'NS' .or.
     +		stcltp ( 1:2 ) .eq. 'SC' .or.
     +		stcltp ( 1:2 ) .eq. 'SF' .or.
     +		stcltp ( 1:2 ) .eq. 'SN' .or.
     +		stcltp ( 1:2 ) .eq. 'ST' .or.
     +		stcltp ( 1:3 ) .eq. 'TCU' ) THEN

	      iscloud = .true.
	   END IF
	END IF

	IF ( .not. iscloud ) RETURN
	
	low = -1
	middle = -1
	high = -1
	lowt = 0
	middlet = 0
	hight = 0

	pos = 1
	IF (lencltp .gt. 500) THEN
	  pos = lencltp
	END IF
	DO WHILE ( pos .lt. lencltp )
	   IF ( stcltp(pos:pos+2) .eq. 'ACC' ) THEN
	      read ( stcltp(pos+3:pos+3), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+3:pos+3), 1000 ) middlec
		 IF ( middlec .gt. middle ) THEN
		    middlet = 8
		    middle = middlec
		 END IF
		 pos = pos + 4
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+2) .eq. 'TCU' ) THEN
	      read ( stcltp(pos+3:pos+3), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+3:pos+3), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 2
		    low = lowc
		 END IF
		 pos = pos + 4
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'AC' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) middlec
		 IF ( middlec .gt. middle ) THEN
		    middlet = 5
		    middle = middlec
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'AS' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) middlec
		 IF ( middlec .gt. middle ) THEN
		    middlet = 1
		    middle = middlec
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'CB' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 9
		    low = lowc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'CU' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 1
		    low = lowc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'CC' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) highc
		 IF ( highc .gt. high ) THEN
		    hight = 9
		    high = highc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'CF' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 7
		    low = lowc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'CI' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) highc
		 IF ( highc .gt. high ) THEN
		    hight = 1
		    high = highc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'CS' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) highc
		 IF ( highc .gt. high ) THEN
		    hight = 8
		    high = highc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'NS' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) middlec
		 IF ( middlec .gt. middle ) THEN
		    middlet = 2
		    middle = middlec
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'SC' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 5
		    low = lowc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'FG' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'SN' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'SF' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 7
		    low = lowc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE IF ( stcltp(pos:pos+1) .eq. 'ST' ) THEN
	      read ( stcltp(pos+2:pos+2), 1001 ) chr
	      IF ( chr .ge. '0' .and. chr .le. '9' ) THEN
		 read ( stcltp(pos+2:pos+2), 1000 ) lowc
		 IF ( lowc .gt. low ) THEN
		    lowt = 6
		    low = lowc
		 END IF
		 pos = pos + 3
	      ELSE
		 pos = lencltp
	      END IF
	   ELSE
	      pos = pos + 3
	      errflg = .true.
	      iret = 14
	   END IF
	END DO
       
	IF ( lowt .ne. -1 ) THEN
	   rivals ( irctyl ) = FLOAT ( lowt )
	END IF
	IF ( middlet .ne. -1 ) THEN
	   rivals ( irctym ) = FLOAT ( middlet )
	END IF
	IF ( hight .ne. -1 ) THEN
	   rivals ( irctyh ) = FLOAT ( hight )
	END IF
C*
1000	FORMAT(i1)
1001	FORMAT(a1)

	RETURN
	END
