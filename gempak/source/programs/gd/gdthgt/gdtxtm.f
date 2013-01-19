	SUBROUTINE GDTXTM ( igdfln, gdatim, npts, timfnd, trange,
     +			    iret )
C************************************************************************
C* GDTXTM								*
C*									*
C* This subroutine sets time information for a contour series.		*
C*									*
C* GDTXTM ( IGDFLN, GDATIM, NPTS, TIMFND, TRANGE, IRET )		*
C*									*
C* Input parameters:							*
C*	IGDFLN		INTEGER		Grid file number		*
C*	GDATIM		CHAR*		User input date/time		*
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		Number of points		*
C*	TIMFND (NPTS)	CHAR*		Time values			*
C*	TRANGE		CHAR*		Actual range of grid times	*
C*	IRET		INTEGER		Return code			*
C*					  1 = user typed RETURN		*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					-13 = no grids found in GDATTIM	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Consolidated from GDTDTA, GDTTTL	*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* K. Brill/GSC          5/90   Made local character strings larger	*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* T.W.Barker/SS/SSD	 8/91	Created from GDTTIM (GDTSER)		*
C* K. Brill/NMC		11/92	Fix ER_WMSG call			*
C* G. Krueger/EAI	 5/93	Modified for use in GDPLOT		*
C* G. Krueger/EAI	 9/93	Modified for use in GDTHGT		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, timfnd (*), trange
	LOGICAL		respnd
C*
	PARAMETER	( NTIME = 200 )
	CHARACTER*36	time, timlst (NTIME), tstrt, tstop, ttt, timout
C------------------------------------------------------------------------

	iret = 0
	npts = 0
C
C*	Get all times which might have data.
C
	CALL GD_GTIM ( igdfln, NTIME, timlst, ntimin, ier )
	IF ( ntimin .eq. 0 ) THEN
	    iret = -13
	    CALL ER_WMSG ( 'GDTHGT', iret, ' ', ier )
	    RETURN
	END IF
C
C*	If GDATIM is 'LIST' and the session is interactive, list the 
C*	times and get a range.
C
	CALL ST_LCUC ( gdatim, time, ier )
	IF ( time .eq. 'LIST' )  THEN
C
	    CALL IP_RESP ( respnd, ier )
	    IF ( respnd )  THEN
		CALL TI_DSPL ( ntimin, timlst, time, ier )
		IF ( ier .eq. 1 )  THEN
		    iret = 1
		    RETURN
		ELSE
		    CALL ST_LCUC ( time, time, ier )
		END IF
C
	    ELSE
		iret   = -4
		CALL ER_WMSG ( 'TI', iret, ' ', ier )
		RETURN
	    END IF
	END IF
C
C*	Narrow the list to that specified in TIME.
C
	CALL TG_FIND ( time, ntimin, timlst, timout, npts, timfnd, ier )
	IF ( ier .ne. 0 )  THEN
	    CALL ER_WMSG ( 'TG', ier, ' ', ier1 )
	    iret = -13
	    RETURN
	END IF
C
C*	Encode the times into the "actual" grid time range (used in
C*	building the default title) . . . reduce the start/stop times
C*	to YYMMDD/HHthh if the other stuff is zero.  Drop A00 if both 
C*	have it, and drop however much of the stop YYMMDD/HH matches 
C*	the start values (starting at the left).
C
	tstrt = timfnd (1)
	tstop = timfnd (npts)
C
C*	The test on character 15 (last digit of 'hhh') ensures that 
C*	the work hasn't already been done on 13:17 ('hhhmm').
C
	IF ( ( tstrt ( 15:15 ) .ne. ' ' ) .and.
     +       ( tstop ( 15:15 ) .ne. ' ' ) )  THEN
	    IF ( ( tstrt ( 16:17 ) .eq. '00' ) .and.
     +	         ( tstop ( 16:17 ) .eq. '00' ) )  THEN
	        tstrt ( 16:17 ) = '  '
	        tstop ( 16:17 ) = '  '
	    END IF
	    IF ( ( tstrt ( 13:13 )   .eq. '0' ) .and.
     +	         ( tstop ( 13:13 )   .eq. '0' ) )  THEN
		ttt   = tstrt
	        tstrt = ttt ( 1:12 ) // ttt ( 14: )
		ttt   = tstop
	        tstop = ttt ( 1:12 ) // ttt ( 14: )
	    END IF
	END IF
C
	IF ( ( tstrt ( 10:11 ) .eq. '00' ) .and.
     +	     ( tstop ( 10:11 ) .eq. '00' ) )  THEN
	    ttt   = tstrt
	    tstrt = ttt ( 1:9 ) // ttt ( 12: )
	    ttt   = tstop
	    tstop = ttt ( 1:9 ) // ttt ( 12: )
	END IF
	CALL ST_LSTR ( tstrt, lstrt, ier )
	IF ( ( tstrt ( lstrt-2:lstrt ) .eq. 'A00' ) .and.
     +	     ( tstop ( lstrt-2:lstrt ) .eq. 'A00' ) )  THEN
	    lstrt = lstrt - 3
	    tstop = tstop ( 1:lstrt )
	ELSE IF ( tstrt ( 1:9 ) .eq. tstop ( 1:9 ) )  THEN
	    tstop = tstop ( 10: )
	END IF
	IF ( tstrt ( 1:6 ) .eq. tstop ( 1:6 ) )  THEN
	    tstop = tstop ( 7: )
	ELSE IF ( tstrt ( 1:4 ) .eq. tstop ( 1:4 ) )  THEN
	    tstop = tstop ( 5: )
	ELSE IF ( tstrt ( 1:2 ) .eq. tstop ( 1:2 ) )  THEN
	    tstop = tstop ( 3: )
	END IF
	trange = tstrt ( 1:lstrt ) // '-' // tstop
C*
	RETURN
	END
