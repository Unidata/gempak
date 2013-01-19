	SUBROUTINE GR_MTTL ( ttlinp, defttl, shrflg, gdtim1, gdtim2,
     +			     ilevfg, level1, level2, ivcord, nparm,
     +			     parm, iscale, gpoint, ttlstr, iret )
C************************************************************************
C* GR_MTTL								*
C*									*
C* This subroutine creates a title to be displayed in the graphics	*
C* programs.  If imbedded within the input title string, characters	*
C* are replaced	as follows:						*
C*		^	Time string					*
C*		~	Valid time string 				*
C*		@	Level string					*
C*		_	Function/parameter string			*
C*		$	Scaling factor string				*
C*		#	Grid point/station string			*
C*		?	Day of the week flag to include with time	*
C*									*
C* GR_MTTL ( TTLINP, DEFTTL, SHRFLG, GDTIM1, GDTIM2, ILEVFG,		*
C*	     LEVEL1, LEVEL2, IVCORD, NPARM, PARM, ISCALE, GPOINT,	*
C*	     TTLSTR, IRET )						*
C*									*
C* Input parameters:							*
C*	TTLINP		CHAR*		Input title string		*
C*	DEFTTL		CHAR*		Default title string		*
C*	SHRFLG		LOGICAL		Short title flag		*
C*	GDTIM1		CHAR*		Grid time 1			*
C*	GDTIM2		CHAR*		Grid time 2			*
C*	ILEVFG		LOGICAL		Level flag			*
C*	LEVEL1		INTEGER		Grid level 1			*
C*	LEVEL2		INTEGER		Grid level 2			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARM(*)		CHAR*12		Grid parameter			*
C*	ISCALE(*)	INTEGER		Scaling factor			*
C*	GPOINT		CHAR*		Grid Point			*
C*									*
C* Output parameters:							*
C*	TTLSTR		CHAR*		Title string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* J. Nielsen/SUNYA	 2/91	Added ^, @, _, and *			*
C* J. Whistler/SSAI	 4/91	Added # for gpoint			*
C* J. Whistler/SSAI	 4/91	Changed scale "*" to "$"		*
C* S. Jacobs/EAI	 9/92	Changed section to get levels and coord	*
C*				to allow the use of any vcoord		*
C* S. Jacobs/EAI	11/92	Added calculation of short title	*
C* S. Jacobs/EAI	 9/93	Added ~ for valid time flag		*
C* S. Jacobs/EAI	 9/93	Added GR_SHRT to compute short title	*
C* S. Jacobs/EAI	10/93	Made GPOINT upper case			*
C* S. Jacobs/NMC	 8/94	Copied from GR_TITL			*
C* S. Jacobs/NMC	 9/94	Added multiple parameter names		*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 5/01	Added day of the week flag		*
C************************************************************************
	CHARACTER*(*)	ttlinp, defttl, gdtim1, gdtim2, parm (*),
     +			gpoint, ttlstr
	INTEGER		iscale (*)
	LOGICAL		shrflg, ilevfg
C*
	CHARACTER	time*80, clev*20, units (5)*2, scale*12
	CHARACTER	ttl*150, ttl2*150, vc*4, timtmp*30, dtim*20
	CHARACTER	ttim*24, prmscl*132, tscal*24, dname (7)*20
	INTEGER		itarr (5)
	LOGICAL		valid, daywk
C
C*			Coord:  PRES, THTA, HGHT, SGMA, DPTH
C
	DATA		units / 'MB', 'K ', 'M ', 'SG', 'M ' /
	DATA		dname / 'SUN', 'MON', 'TUE', 'WED',
     +				'THU', 'FRI', 'SAT' /
C------------------------------------------------------------------------
	iret = 0
	IF  ( ttlinp .eq. ' ' )  THEN
	    ttl = defttl
	ELSE
	    ttl = ttlinp
	END IF
C
C*	Check for the day of the week flag.
C
	CALL ST_RMST  ( ttl, '?', iqpos, ttl2, iret )
	ttl = ttl2
	IF  ( iqpos .gt. 0 )  THEN
	    daywk = .true.
	  ELSE
	    daywk = .false.
	END IF
C
C*	See if we need to construct a time string.
C
	CALL ST_RMST  ( ttl, '^', itpos, ttl2, iret )
	ttl = ttl2
	CALL ST_RMST  ( ttl, '~', idpos, ttl2, iret )
	ttl = ttl2
C
	IF  ( itpos .gt. idpos )  THEN
	    ipos  = itpos
	    valid = .false.
	ELSE IF  ( idpos .gt. itpos )  THEN
	    ipos  = idpos
	    valid = .true.
	ELSE
	    ipos  = 0
	END IF
C
	IF  ( ( ipos .ne. 0 ) .and. ( gdtim1 .ne. ' ' ) )  THEN
C
C*	    Encode the date/time part of the title and get its length.
C*	    Encode the first time and get length.
C
	    IF  ( valid )  THEN
		CALL TG_VALD ( gdtim1, time, ier )
		IF  ( ier .ne. 0 )  time = gdtim1
	    ELSE
		time = gdtim1
	    END IF
	    IF  ( shrflg )  THEN
		ttim = time (5:9) // time (12:15)
		time = ttim
	      ELSE
		IF  ( daywk )  THEN
		    CALL TG_VALD ( gdtim1, dtim, ier )
		    IF  ( ier .ne. 0 )  dtim = gdtim1
		    CALL TI_CTOI ( dtim, itarr, ier )
		    CALL TI_DAYW ( itarr, iday, ier )
		    CALL ST_LSTR ( dname(iday), lend, ier )
		    dtim = dname(iday) (:lend) // ' ' // time
		    time = dtim
		END IF
	    END IF
	    CALL ST_LSTR  ( time, lent, ier )
C
C*	    If the second time is not blank, add it to the string.
C*	    Separate the two times with a colon.
C
	    IF  ( gdtim2 .ne. ' ' )  THEN
		time ( lent+1: ) = ' : '
		lent = lent + 3
		IF  ( valid )  THEN
		    CALL TG_VALD ( gdtim2, timtmp, ier )
		    IF  ( ier .ne. 0 )  timtmp = gdtim2
		ELSE
		    timtmp = gdtim2
		END IF
		IF  ( shrflg )  THEN
		    ttim = timtmp (5:9) // timtmp (12:15)
		    timtmp = ttim
		  ELSE
		    IF  ( daywk )  THEN
			CALL TG_VALD ( gdtim2, dtim, ier )
			IF  ( ier .ne. 0 )  dtim = gdtim2
			CALL TI_CTOI ( dtim, itarr, ier )
			CALL TI_DAYW ( itarr, iday, ier )
			CALL ST_LSTR ( dname(iday), lend, ier )
			dtim = dname(iday) (:lend) // ' ' // timtmp
			timtmp = dtim
		    END IF
		END IF
		time ( lent+1: ) = timtmp
		IF  ( shrflg )  THEN
		    CALL ST_RMBL  ( time, time, lent, ier )
		ELSE
		    CALL ST_LSTR  ( time, lent, ier )
		END IF
	    END IF
	    IF  ( lent .eq. 0 )  lent = 1
	    IF  ( ipos .eq. 1 )  THEN
		ttl2 = time ( 1:lent ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ipos-1 ) // time ( 1:lent ) // 
     +		       ttl ( ipos : )
	    END IF
	    ttl = ttl2
	END IF
C
C*	See if we need to construct a level string.
C
	CALL ST_RMST  ( ttl, '@', ilpos, ttl2, iret )
	ttl = ttl2
	clev = ' '
	lenl = 0
	IF  ( ( ilpos .ne. 0 ) .and. ( ilevfg ) )  THEN
C
C*	    Use the string 'SFC' if the vertical coordinate is 0;
C*          or if there is only one level and it is surface data,
C*	    use 'SFC'.
C
	    IF  ( ( ivcord .eq. 0 ) .or.
     +		  ( ( level1 .eq. 0 ) .and.
     +		    ( level2 .eq. -1 ) ) )  THEN
		clev = 'SFC'
		lenl = 3
C
C*	  	When vertical coordinate is not zero, encode the
C*		first level.
C
	    ELSE IF ( ivcord .gt. 0 )  THEN
		CALL ST_INCH ( level1, clev, ier )
		IF  ( ier .eq. 0 )  THEN
		    CALL ST_LSTR ( clev, lenl, ier )
		ELSE
		    lenl = 1
		    clev = ' '
		END IF
C
C*	        Encode the second level if it is present, i.e. <> -1.
C
		IF  ( level2 .ne. -1 )  THEN
		    clev(lenl+1:) = ' : '
		    CALL ST_INCH ( level2, clev(lenl+4:), ier )
		    IF  ( ier .ne. 0 )  THEN
			clev(lenl+1:) = ' '
		    ELSE
			CALL ST_LSTR ( clev, lenl, ier )
		    END IF
		END IF
C
C*	    	Add the units at the end of the string.
C
		IF  ( ivcord .le. 5 )  THEN
		    clev(lenl+2:) = units(ivcord)
		ELSE
C
C*		    Make sure that the coord is at least ONE, PRINTABLE
C*		    character in the ASCII set.
C
		    IF  ( ivcord .gt. 32 )  THEN
			CALL ST_ITOS ( ivcord, 1, nchar, vc, ier )
			IF  ( ier .ne. 0 ) vc = ' '
			clev(lenl+2:) = vc
		    ELSE
			clev(lenl+2:) = ' '
		    END IF
		END IF
		IF  ( shrflg )  THEN
		    CALL ST_RMBL  ( clev, clev, lenl, ier )
		ELSE
		    CALL ST_LSTR  ( clev, lenl, ier )
		END IF
	    END IF
C
	    IF  ( lenl .eq. 0 )  lenl = 1
	    IF  ( ilpos .eq. 1 )  THEN
		ttl2 = clev ( 1:lenl ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ilpos-1 ) // clev ( 1:lenl ) // 
     +		       ttl ( ilpos : )
	    END IF
	    ttl = ttl2
	END IF
C
C*	Combine the arrays of parameters and scales.
C
	If  ( nparm .gt. 1 )  THEN
	    prmscl = ' '
	    lena   = 0
	    DO  k = 1, nparm
		CALL ST_LSTR ( parm (k), lenp, ier )
		IF  ( parm (k) (:4) .ne. 'BLNK' )  THEN
		    IF  ( iscale (k) .ne. 0 )  THEN
			CALL ST_INLN ( iscale (k), tscal, lens, ier )
			prmscl ( lena+2: ) = parm (k)( :lenp ) //
     +					     ' (*10**' //
     +					     tscal ( :lens ) // ') '
		    ELSE
			prmscl ( lena+2: ) = parm (k)( :lenp ) // ' '
		    END IF
		END IF
		CALL ST_LSTR ( prmscl, lena, ier )
	    END DO
	ELSE
	    prmscl = parm (1)
	END IF
C
C*	See if we need to use the grid function string.
C
	CALL ST_RMST  ( ttl, '_', ippos, ttl2, iret )
	ttl = ttl2
	IF  ( ( ippos .ne. 0 )  .and. ( prmscl .ne. ' ' ) )  THEN
C
C*	    Get length of grid function name.
C
	    CALL ST_LSTR  ( prmscl, lenp, ier )
	    IF  ( lenp .eq. 0 )  lenp = 1
	    IF  ( ippos .eq. 1 )  THEN
		ttl2 = prmscl ( 1:lenp ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ippos-1 ) // prmscl ( 1:lenp ) // 
     +		       ttl ( ippos: )
	    END IF
	    ttl = ttl2
	END IF
C
C*	See if we need to construct a scaling string.
C
	CALL ST_RMST  ( ttl, '$', ispos, ttl2, iret )
	ttl = ttl2
	jscale = iscale(1)
	IF  ( ( ispos .ne. 0 ) .and. ( .not. shrflg ) .and.
     +	      ( nparm .eq. 1 ) )  THEN
C
C*	    Encode the scaling term if present.
C
	    IF  ( jscale .eq. 0 )  THEN
		scale = ' '
		lens = 0
	    ELSE
		scale = ' (*10**'
		CALL ST_INCH  ( iscale, scale (8:), ier )
		IF  ( ier .ne. 0 )  THEN
		    scale = ' '
		    lens = 0
		ELSE
		    CALL ST_LSTR  ( scale, lens, ier )
		    scale ( lens+1: ) = ')'
		    lens = lens + 1
		END IF
	    END IF
	    IF  ( lens .eq. 0 )  lens = 1
	    IF  ( ispos .eq. 1 )  THEN
		ttl2 = scale ( 1:lens ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ispos-1 ) // scale ( 1:lens ) // 
     +		       ttl ( ispos : )
	    END IF
	    ttl = ttl2
	END IF
C
C*	See if we need to use the grid point string.
C
	CALL ST_RMST  ( ttl, '#', igpos, ttl2, iret )
	ttl = ttl2
	leng = 0
	IF  ( ( igpos .ne. 0 ) .and. ( gpoint .ne. ' ' ) )  THEN
C
C*	    Get length of grid function name.
C
	    CALL ST_LCUC  ( gpoint, gpoint, ier )
	    CALL ST_LSTR  ( gpoint, leng, ier )
	    IF  ( leng .eq. 0 )  leng = 1
	    IF  ( igpos .eq. 1 )  THEN
		ttl2 = gpoint ( 1:leng ) // ttl
	    ELSE
		ttl2 = ttl ( 1:igpos-1 ) // gpoint ( 1:leng ) // 
     +		       ttl ( igpos : )
	    END IF
	    ttl = ttl2
	END IF
C
C*	Put the string together.
C
	ttlstr = ttl
C*
	RETURN
	END
