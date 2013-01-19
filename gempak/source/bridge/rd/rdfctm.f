	SUBROUTINE RD_FCTM ( segmnt, lens, isstar, idtarr, idp,
     +			     mxfctm, ispnt, gemftm, lfchr, kpshr,
     +			     jftmst, jftmen, iret )
C************************************************************************
C* RD_FCTM								*
C*									*
C* This subroutine finds and decodes the forecast time string and	*
C* converts it into GEMPAK format.					*
C*									*
C* RD_FCTM  ( SEGMNT, LENS, ISSTAR, IDTARR, IDP, MXFCTM, ISPNT, GEMFTM,	*
C*	      LFCHR, KPSHR, JFTMST, JFTMEN, IRET )			*
C*									*
C* Input parameters:							*
C*      SEGMNT		CHAR*		Bulletin segment		*
C*      LENS		INTEGER		Length of segment		*
C*	ISSTAR (5)	INTEGER		Integer issue time array	*
C*	IDTARR (5)	INTEGER		Integer time array for 1st date	*
C*	IDP		INTEGER		Position of 1st date in dateline*
C*					 or if <= 0, then using pos of	*
C*					 1st fct hr and idtarr has last	*
C*					 date and |idp| is local fcst hr*
C*	MXFCTM		INTEGER		Maximum number of forecast times*
C*									*
C* Input and output parameters:						*
C*	ISPNT		INTEGER		Segment pointer			*
C*									*
C* Output parameters:							*
C*      GEMFTM (*)	CHAR*		GEMPAK date/time		*
C*      LFCHR (*)	INTEGER		Local forecast hours		*
C*      KPSHR (*)	INTEGER		Positions of forecast hours	*
C*      JFTMST		INTEGER		Index of first valid GEMFTM	*
C*      JFTMEN		INTEGER		Index of last GEMFTM		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = no time line found	*
C*					  -2 = error in time line	*
C*					  -3 = no more report lines	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	Handle "3HRLY" & "6HRLY" and 2 timelines* 
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	segmnt, gemftm (*)
	INTEGER		isstar(*), idtarr (*) 
	INTEGER		lfchr(*), kpshr(*)
C*
        CHARACTER       tznlst(8)*4, znlst(8)*5
        INTEGER         ilens(9), jdar(5), jfdar(5), mdar(5)
        DATA            tznlst / 'EDT ', 'EST ', 'CDT ', 'CST ',
     +				 'MDT ', 'MST ', 'PDT ', 'PST '/
        DATA            ilens / 9*5 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Find the start of time line
C
	DO k = 1, 8
	    znlst (k) = CHLF // tznlst (k)
	END DO
	
    	CALL ST_NXTS ( segmnt, ispnt, lens, znlst, ilens, 8, ishp,
     +		istrg, ier )
	ishpsv = ishp
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Check for possible "3HRLY" and "6HRLY" after time zone
C
	iskip = 4
	iphr = INDEX ( segmnt ( ishp + 3:ishp + 20 ), '3HRLY' ) 
	IF ( iphr .eq. 0 ) THEN
	    iphr = INDEX ( segmnt ( ishp + 3:ishp + 20 ), '6HRLY' )
	    IF ( iphr .ne. 0 ) THEN
		ishp = iphr + ishp + 2
		iskip = 5
	    END IF
	  ELSE
	    ishp = iphr + ishp + 2
	    iskip = 5
	END IF
C
C*	Get local times and positions
C
    	ishp = ishp + 1
	iehp = INDEX ( segmnt ( ishp:ishp + 80 ), CHCR )
	ispnt = ishp + iehp
	IF ( iehp .eq. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
	jb = ishp + iskip 
	ip = 0
	jftmen = 0
	DO WHILE ( jb .lt. ishp + iehp - 1 .and. jftmen .le. mxfctm )
	    DO WHILE ( INDEX ( segmnt ( jb:jb ), ' ' ) .ne. 0 
     +			.and. jb .lt. ishp + iehp - 1)
		jb = jb + 1
	    END DO
		js = jb
		jn = jb + 1
	    DO WHILE ( INDEX ( segmnt ( jn:jn ), ' ' ) .eq. 0
     +			.and. jn .lt. ishp + iehp - 1)
		jn = jn + 1
	    END DO
	    IF ( jn - jb .ne. 2 ) THEN
		IF ( segmnt (jb:jb) .ne. CHCR ) THEN
	            iret = -2
	            RETURN
		END IF
	      ELSE		
	        jftmen = jftmen + 1
	        CALL ST_NUMB ( segmnt (jb:jn-1), lfchr (jftmen), ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -2
		    RETURN
		END IF
C*
C*		Change hour 24 to 00 for consistency
C*
		IF ( lfchr (jftmen) .eq. 24 ) lfchr (jftmen) = 0
	        kpshr (jftmen) = js - ishpsv
	    END IF
	    jb = jn + 1
	END DO
	IF ( jftmen .eq. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Convert to GEMPAK format
C*
C*	If idp is greater than 0 (not extended fcst time line),
C*	determine the position of the known hour for the first date
C*	(ie, the forecast hour that is closest to the date).
C*
C*	If idp is less than or equal to 0 (extended fcst time line),
C*	then the absolute value of idp is the last fcst hour (local
C*	time) in the first time line and idtarr contains the integer
C*	time array of that forecast hour, which can be used.
C
	IF ( idp .gt. 0 ) THEN 
	    jj = 1
	    jfd = 0
	    DO WHILE ( jj .lt. jftmen .and. jfd .eq. 0 )
	        IF ( kpshr (jj) .lt. idp ) THEN
		    jj = jj + 1
	          ELSE
		    jfd = jj 
	        END IF
	    END DO
C
C*	    Determine GEMPAK time for the known hour at index jfd
C
	    jdar ( 1 ) = idtarr ( 1 )
	    jdar ( 2 ) = idtarr ( 2 )
	    jdar ( 3 ) = idtarr ( 3 )
	    jdar ( 4 ) = lfchr(jfd)
	    jdar ( 5 ) = 0
	  ELSE IF ( idp .le. 0 ) THEN
            jfd = 0
C
C*	    Convert known time from GMT to local time.
C
	    CALL TI_TZDF ( idtarr, 'GMT', tznlst(istrg)(1:3), jdar,
     +			hr, ier )
	END IF

	IF ( jfd .eq. 0 .and. idp .gt. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Determine GEMPAK time for the known hour at index jfd.
C*	First convert local time to GMT.
C
	CALL TI_TZDF ( jdar, tznlst(istrg)(1:3), 'GMT', jfdar, hr, ier )
C
C*	Convert remaining local times to GEMPAK
C*	First convert times before index jfd.
C
	IF ( idp .gt. 0 ) THEN
C
C*	    Convert to GEMPAK format
C
    	    CALL TI_ITOC ( jfdar, gemftm(jfd), ier )
	    DO jj = 1,5
	        mdar ( jj ) = jfdar ( jj )
	    END DO
	    m2 = jfd
	    m1 = m2 - 1
	    DO WHILE ( m1 .ge. 1 ) 
	        mdif = lfchr ( m2 ) - lfchr ( m1 )
	        IF ( mdif .lt. 0 ) THEN
		    mdif = ( 24 + mdif )	    
	        END IF
	        mdif = mdif * 60
	        CALL TI_SUBM ( mdar, mdif, mdar, ier )
C
C*	        Convert to GEMPAK format
C
	        CALL TI_ITOC ( mdar , gemftm( m1 ), ier )
	        m2 = m1
	        m1 = m1 - 1
	    END DO
	END IF
C
C*	Now convert local times after index jfd to GEMPAK
C
        DO jj = 1,5
	    mdar ( jj ) = jfdar ( jj )
        END DO
	m1 = jfd
	m2 = m1 + 1
	DO WHILE ( m2 .le. jftmen )
	    IF ( m1 .eq. 0 ) THEN
		mdif = lfchr ( m2 ) + idp
	      ELSE
	        mdif = lfchr ( m2 ) - lfchr ( m1 )
	    END IF
	    IF ( mdif .lt. 0 ) THEN
		mdif = ( 24 + mdif )	    
	    END IF
	    mdif = mdif * 60
	    CALL TI_ADDM ( mdar, mdif, mdar, ier )
C
C*	    Convert to GEMPAK format
C
	    CALL TI_ITOC ( mdar, gemftm( m2 ), ier )
	    m1 = m2
	    m2 = m2 + 1
	END DO
C
C*	Find the first valid time index (at or after issue time)
C
	jftmst = 0
	jj = 1
	DO WHILE ( jj .le. jftmen .and. jftmst .eq. 0 )
	    CALL TI_CTOI ( gemftm ( jj ), mdar, ier )
	    CALL TI_MDIF ( isstar, mdar, mdiff, ier ) 
	    IF ( mdiff .le. 0 ) THEN
		jftmst = jj
	    END IF
	    jj = jj + 1
	END DO
C
C*	If there is a second forecast time string, skip over it.
C*	(UTC time strings are not processed, because local times
C*	are needed for MX/MN.)
C 
	CALL ST_NXTS ( segmnt, ispnt, ispnt + 30, znlst, ilens, 8,
     +		       ipos, istrg, ier )
	IF ( ier .eq. 0 ) THEN
	    idx = INDEX ( segmnt ( ipos:ipos + 80 ), CHCR )
	    IF ( idx .gt. 0 ) THEN
		ispnt = ispnt + idx
	      ELSE
	        iret = -3
	    END IF
	END IF
C*
	RETURN
	END
