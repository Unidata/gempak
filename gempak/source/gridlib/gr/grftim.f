	SUBROUTINE GR_FTIM ( gdfile, gdatim, timfnd, ntime, trange,
     +			     iret )
C************************************************************************
C* GR_FTIM								*
C*									*
C* This subroutine returns an array of grid times based on GDFILE	*
C* and GDATIM. 								*
C*									*
C* GR_FTIM ( GDFILE, GDATIM, TIMFND, NTIME, TRANGE, IRET )		*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		GEMPAK grid file or alias	*
C*	GDATIM		CHAR*		Grid time			*
C*									*
C* Output parameters:							*
C*	TIMFND(NTIME)	CHAR*	 	GEMPAK grid time		*
C*	NTIME		INTEGER		Number of times returned	*
C*	TRANGE		CHAR*		Range of grid times		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                      -20 = no times selected		*
C**									*
C* Log:									*
C* T. Lee/GSC		 6/01	Created					*
C* T. Lee/GSC		 7/01	Processed two grid times field		*
C* T. Lee/SAIC		 9/01	Standardized cycle			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
 	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, gdatim, timfnd(*), trange
C
	CHARACTER	filnam*128, farr(2)*128
	CHARACTER	cycle*40, file(2)*128, gfile*128
	CHARACTER	gtime*20, atime*20, btime*20
	CHARACTER*36	tstrt, tstop, ttt, ndttm
        CHARACTER       cyclst*(LLMXGT*41), cycles(LLMXGT)*40
	INTEGER		itype
C-----------------------------------------------------------------------
C
	iret = 0
C
C*      Parse file name.
C
	CALL ST_CLST ( gdfile, '+', ' ', 2, file, nf, iret )
	IF  ( nf .ge. 2 )  THEN
	    gfile = file (1)
	  ELSE
	    gfile = gdfile
	END IF
C
	CALL ST_CLST ( gfile, '|', ' ', 2, farr, nfarr, iret )
	filnam = farr (1)
	IF  ( filnam .ne. ' ' )  THEN
	    IF  ( nfarr .eq. 2 )  THEN
		cycle  = farr (2)
	      ELSE
		CALL GD_GCYC ( filnam, ';', ncyc, cyclst, iret )
		CALL ST_CLSL ( cyclst, ';', ' ', ncyc, cycles, nc, iret)
		cycle = cycles (nc)
	    END IF
	    itype = 1
	    CALL CSS_GTIM ( itype, ndttm, ier )
	    CALL TI_STAN ( cycle, ndttm, cycle, ier )
	END IF
C
C*	Handle two times.
C
	ipt = INDEX ( gdatim, ':' )
	IF  ( ipt .gt. 0 )  THEN
	    CALL GR_TMFL  ( gdatim, filnam, cycle, LLMXGT, ntime,
     +			    timfnd, iret )
	    IF  ( iret .ne. 0 )  THEN
	        gtime = gdatim ( : ipt - 1 )
	        CALL GR_TMFL  ( gtime, filnam, cycle, LLMXGT, ntime,
     +				timfnd, iret )
		IF  ( iret .eq. 0 )  THEN
		    atime = timfnd ( ntime )
C
		    gtime = gdatim ( ipt + 1 : )
		    CALL GR_TMFL  ( gtime, filnam, cycle, LLMXGT, ntime,
     +				    timfnd, iret )
		    IF  ( iret .eq. 0 )  THEN
			btime =  timfnd ( ntime )
		      ELSE
			btime = gtime
		    END IF
	          ELSE
		    atime = gtime
		END IF
C
		ntime = 1
		timfnd ( ntime ) = atime // ':' // btime
	    END IF
	  ELSE
	    CALL GR_TMFL  ( gdatim, filnam, cycle, LLMXGT, ntime,
     +			    timfnd, iret )
	END IF
C
C*	Encode the times into the "actual" grid time range (used in
C*	building the default title) . . . reduce the start/stop times
C*	to YYMMDD/HHthh if the other stuff is zero.  Drop A00 if both
C*	have it, and drop however much of the stop YYMMDD/HH matches
C*	the start values (starting at the left).
C
        tstrt = timfnd (1)
	tstop = timfnd (ntime)
C
C*	The test on character 15 (last digit of 'hhh') ensures that
C*	the work hasn't already been done on 13:17 ('hhhmm').
C
	IF  ( ( tstrt ( 15:15 ) .ne. ' ' ) .and.
     +        ( tstop ( 15:15 ) .ne. ' ' ) )  THEN
	    IF  ( ( tstrt ( 16:17 ) .eq. '00' ) .and.
     +            ( tstop ( 16:17 ) .eq. '00' ) )  THEN
		tstrt ( 16:17 ) = '  '
		tstop ( 16:17 ) = '  '
	    END IF
	    IF  ( ( tstrt ( 13:13 )   .eq. '0' ) .and.
     +            ( tstop ( 13:13 )   .eq. '0' ) )  THEN
		ttt   = tstrt
		tstrt = ttt ( 1:12 ) // ttt ( 14: )
		ttt   = tstop
		tstop = ttt ( 1:12 ) // ttt ( 14: )
	    END IF
	END IF
C
	IF  ( ( tstrt ( 10:11 ) .eq. '00' ) .and.
     +        ( tstop ( 10:11 ) .eq. '00' ) )  THEN
	    ttt   = tstrt
	    tstrt = ttt ( 1:9 ) // ttt ( 12: )
	    ttt   = tstop
	    tstop = ttt ( 1:9 ) // ttt ( 12: )
	END IF
	CALL ST_LSTR  ( tstrt, lstrt, ier )
	IF  ( ( tstrt ( lstrt-2:lstrt ) .eq. 'A00' ) .and.
     +        ( tstop ( lstrt-2:lstrt ) .eq. 'A00' ) )  THEN
	    lstrt = lstrt - 3
	    tstop = tstop ( 1:lstrt )
	  ELSE IF  ( tstrt ( 1:9 ) .eq. tstop ( 1:9 ) )  THEN
	    tstop = tstop ( 10: )
	END IF
	IF  ( tstrt ( 1:6 ) .eq. tstop ( 1:6 ) )  THEN
	    tstop = tstop ( 7: )
	  ELSE IF  ( tstrt ( 1:4 ) .eq. tstop ( 1:4 ) )  THEN
	    tstop = tstop ( 5: )
	  ELSE IF  ( tstrt ( 1:2 ) .eq. tstop ( 1:2 ) )  THEN
	    tstop = tstop ( 3: )
	END IF
	trange = tstrt ( 1:lstrt ) // '-' // tstop
C
	RETURN
C
	END
