	SUBROUTINE GR_SHRT ( shrtin, gdtime, ilevfg, level, ivcord,
     +			     parm, iscale, gpoint, shrttl, iret )
C************************************************************************
C* GR_SHRT								*
C*									*
C* This subroutine creates a default short title for the contouring	*
C* program. If imbedded within the input short title string,		*
C* characters are replaced as follows:					*
C*		^	Time string					*
C*		@	Grid level string				*
C*		_	Grid function string				*
C*		$	Scaling factor string				*
C*		#	Grid point string				*
C*									*
C*		~	Valid time flag					*
C*									*
C* GR_SHRT  ( SHRTIN, GDTIME, ILEVFG, LEVEL, IVCORD, PARM, ISCALE,	*
C*	      GPOINT, SHRTTL, IRET )					*
C*									*
C* Input parameters:							*
C*	SHRTIN		CHAR*		Input short title string	*
C*	GDTIME (2)	CHAR*		Grid time			*
C*	ILEVFG		LOGICAL		Level flag			*
C*	LEVEL  (2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*12		Grid parameter			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	GPOINT		CHAR*		Grid Point			*
C*									*
C* Output parameters:							*
C*	SHRTTL		CHAR*		Short title string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 9/93		Copied from GR_TITL		*
C* S. Jacobs/NMC         4/94  		Removed unused variables	*
C************************************************************************
	CHARACTER*(*)	gdtime (2), parm, shrtin, gpoint, shrttl
	INTEGER		level (2)
	LOGICAL		ilevfg, valid
C*
	CHARACTER	time*30, clev*20, units (5)*2, scale*12
	CHARACTER	ttl*150, ttl2*150, vc*4, timtmp*30
	LOGICAL		fill
C
C*			Coord:  PRES, THTA, HGHT, SGMA, DPTH
C
	DATA		units / 'MB', 'K ', 'M ', 'SG', 'M ' /
C------------------------------------------------------------------------
	iret = 0
C
C*      Check for valid time flag.
C
	valid = .false.
	IF  ( shrtin (1:1) .eq. '~' )  THEN
	    shrtin = shrtin (2:)
	    valid = .true.
	END IF
C
C*	Load the input short title string into the working string
C
	ttl = shrtin
C
C*	Check for no input short title	
C
	fill = .false.
	IF  ( ttl .eq. ' ' )  THEN 
	  fill = .true.
	END IF
C
C*	See if we need to construct a time string.
C
	itpos = INDEX ( ttl, '^' )
	CALL ST_RMST  ( ttl, '^', itpos, ttl2, iret )
	ttl = ttl2
	IF  ( ( ( fill ) .or. ( itpos .ne. 0 ) ) .and.
     +			      ( gdtime (1) .ne. ' ' ) )  THEN
C
C*	  Encode the date/time part of the title and get its length.
C*	  Encode the first time and get length.
C
	  IF  ( valid )  THEN
	      CALL TG_VALD ( gdtime(1), time, ier )
	  ELSE
	      time = gdtime (1)
	  END IF
	  time = time (5:9) // time (12:15)
	  CALL ST_LSTR  ( time, lent, ier )
C
C*	  If the second time is not blank, add it to the string.
C*	  Separate the two times with a colon.
C
	  IF  ( gdtime (2) .ne. ' ' )  THEN
	    time ( lent+1: ) = ':'
	    lent = lent + 1
	    IF  ( valid )  THEN
	        CALL TG_VALD ( gdtime(2), timtmp, ier )
	    ELSE
	        timtmp = gdtime (2)
	    END IF
	    time ( lent+1: ) = timtmp (5:9) // timtmp (12:15)
	    CALL ST_LSTR  ( time, lent, ier )
	  END IF
	  IF  ( lent .eq. 0 )  lent = 1
	  IF  ( .not. fill )  THEN
	    IF  ( itpos .eq. 1 )  THEN
		ttl2 = time ( 1:lent ) // ttl
	    ELSE
		ttl2 = ttl ( 1:itpos-1 ) // time ( 1:lent ) // 
     +		       ttl ( itpos : )
	    END IF
		ttl = ttl2
	  END IF
	END IF
C
C*	See if we need to construct a level string.
C
 	ilpos = INDEX ( ttl, '@' )
	CALL ST_RMST  ( ttl, '@', ilpos, ttl2, iret )
	ttl = ttl2
	clev = ' '
	lenl = 0
	IF  ( ( ( fill ) .or. ( ilpos .ne. 0 ) ) .and.
     +			      ( ilevfg ) )  THEN
C
C*	Use the string 'SFC' if the vertical coordinate is 0;
C*      or if there is only one level and it is surface data, use 'SFC'.
C
	  IF  ( ( ivcord .eq. 0 ) .or.
     +		( ( level(1) .eq. 0 ) .and.
     +		  ( level(2) .eq. -1 ) ) )  THEN
	    clev = 'SFC'
	    lenl = 3
C
C*	  When vertical coordinate is not zero, encode the first level.
C
	  ELSE IF ( ivcord .gt. 0 )  THEN
	    CALL ST_INCH ( level(1), clev, ier )
	    IF  ( ier .eq. 0 )  THEN
		CALL ST_LSTR ( clev, lenl, ier )
	    ELSE
		lenl = 1
		clev = ' '
	    END IF
C
C*	    Encode the second level if it is present, i.e. <> -1.
C
	    IF  ( level(2) .ne. -1 )  THEN
		clev(lenl+1:) = ':'
		CALL ST_INCH ( level(2), clev(lenl+2:), ier )
		IF  ( ier .ne. 0 )  THEN
		    clev(lenl+1:) = ' '
		ELSE
		    CALL ST_LSTR ( clev, lenl, ier )
		END IF
	    END IF
C
C*	    Add the units at the end of the string.
C
	    IF  ( ivcord .le. 5 )  THEN
		clev(lenl+1:) = units(ivcord)
	    ELSE
C
C*		Make sure that the coord is at least ONE, PRINTABLE
C*		character in the ASCII set.
C
		IF  ( ivcord .gt. 32 )  THEN
		    CALL ST_ITOS ( ivcord, 1, nchar, vc, ier )
		    IF  ( ier .ne. 0 ) vc = ' '
		    clev(lenl+1:) = vc
		ELSE
		    clev(lenl+1:) = ' '
		END IF
	    END IF
	    CALL ST_LSTR ( clev, lenl, ier )
	  END IF
C
	  IF  ( lenl .eq. 0 )  lenl = 1
	  IF  ( .not. fill )  THEN
	    IF  ( ilpos .eq. 1 )  THEN
		ttl2 = clev ( 1:lenl ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ilpos-1 ) // clev ( 1:lenl ) // 
     +		       ttl ( ilpos : )
	    END IF
		ttl = ttl2
	  END IF
	END IF
C
C*	See if we need to use the grid function string.
C
 	ippos = INDEX ( ttl, '_' )
	CALL ST_RMST  ( ttl, '_', ippos, ttl2, iret )
	ttl = ttl2
	IF  ( ( ( fill ) .or. ( ippos .ne. 0 ) ) .and.
     +			      ( parm .ne. ' ' ) )  THEN
C
C*	  Get length of grid function name.
C
	  CALL ST_LSTR  ( parm, lenp, ier )
	  IF  ( lenp .eq. 0 )  lenp = 1
	  IF  ( .not. fill )  THEN
	    IF  ( ippos .eq. 1 )  THEN
		ttl2 = parm ( 1:lenp ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ippos-1 ) // parm ( 1:lenp ) // 
     +		       ttl ( ippos: )
	    END IF
		ttl = ttl2
	  END IF
	END IF
C
C*	See if we need to construct a scaling string.
C
 	ispos = INDEX ( ttl, '$' )
	CALL ST_RMST  ( ttl, '$', ispos, ttl2, iret )
	ttl = ttl2
	IF  ( ispos .ne. 0 )  THEN
C
C*	  Encode the scaling term if present.
C
	  IF  ( iscale .eq. 0 )  THEN
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
	  IF  ( .not. fill )  THEN
	    IF  ( ispos .eq. 1 )  THEN
		ttl2 = scale ( 1:lens ) // ttl
	    ELSE
		ttl2 = ttl ( 1:ispos-1 ) // scale ( 1:lens ) // 
     +		       ttl ( ispos : )
	    END IF
		ttl = ttl2
	  END IF
	ELSE
	  scale = ' '
	  lens = 1
	END IF
C
C*	See if we need to use the grid point string.
C
	igpos = INDEX ( ttl, '#' )
	CALL ST_RMST  ( ttl, '#', igpos, ttl2, iret )
	ttl = ttl2
	leng = 0
	IF  ( ( ( fill ) .or. ( igpos .ne. 0 ) ) .and.
     +			      ( gpoint .ne. ' ' ) )  THEN
C
C*	  Get length of grid function name.
C
	  CALL ST_LSTR  ( gpoint, leng, ier )
	  IF  ( leng .eq. 0 )  leng = 1
	  IF  ( .not. fill )  THEN
	    IF  ( igpos .eq. 1 )  THEN
		ttl2 = gpoint ( 1:leng ) // ttl
	    ELSE
		ttl2 = ttl ( 1:igpos-1 ) // gpoint ( 1:leng ) // 
     +		       ttl ( igpos : )
	    END IF
		ttl = ttl2
	  END IF
	END IF
C
C*	Put the string together.
C
	IF  ( fill )  THEN
	  shrttl = time (1:lent) // ' ' // clev (1:lenl) // ' ' //
     +		 parm (1:lenp) // scale (1:lens) // ' ' //
     +		 gpoint (1:leng)
	ELSE
	  shrttl = ttl
	END IF
C*
	RETURN
	END
