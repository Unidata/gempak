	SUBROUTINE GG_LTRP ( lunf, dattm2, datfil, datgrp, ntimes, 
     +			     icolrs, mark, szmrkr, iflgps, iflgng,
     +			     iret )
C************************************************************************
C* GG_LTRP								*
C*									*
C* This subroutine reads and plots lightning data from a single hourly  *
C* file, using speciifed colors and time increments.	                *
C*									*
C* GG_LTRP ( LUNF, DATTM2, DATFIL, DATGRP, NTIMES, ICOLRS, MARK,        *
C*           SZMRKR, IFLGPS, IFLGNG, IRET )                             *
C*                                                                      *
C* Input parameters:							*
C*	LUNF		INTEGER		Logical unit number for input   *
C*	DATTM2		CHAR*		GEMPAK ending time for data     *
C*	DATFIL		CHAR*		GEMPAK time for current data    *
C*	DATGRP(*)	CHAR*		GEMPAK break times for colors   *
C*	NTIMES		INTEGER 	Number of break times           *
C*	ICOLRS(*)	INTEGER		Color for each time increment   *
C*	MARK(*)		INTEGER		Marker number, flag, line width *
C*	SZMRKR(*)	REAL		Marker size multiplier          *
C*	IFLGPS		INTEGER		Positive flash flag		*
C*	IFLGNG		INTEGER		Negative flash flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/00	                                        *
C* S. Jacobs/NCEP	 3/00	Added Pos and Neg flags as input	*
C* S. Jacobs/NCEP	 3/00	Changed 0 polarity to use negative sym	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattm2, datfil, datgrp (*)
	INTEGER		icolrs (*), mark (*)
	REAL		szmrkr (*)
C*
	PARAMETER	( MAXSTK = LLMXPT * 2 )
C*
	CHARACTER	datmin*20, cmin*2
	INTEGER		idata (LLMXPT*4), iptr (0:62),
     +			istrtp (0:60), istrtn (0:60), icolor (0:60)
	REAL		rlat (MAXSTK,2), rlon (MAXSTK,2)
	LOGICAL		found
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get the pointers for each minute, and the header
C*	information.
C
	CALL LW_INPT ( lunf, 0, 63, iptr, ier )
	lmin = iptr ( 61 )
	ipos = 0
	ineg = 0
C
C*	Loop over all minutes in the file.
C
	ii = 0
	DO WHILE ( ii .le. lmin )
	    istrtp ( ii ) = IMISSD
	    istrtn ( ii ) = IMISSD
	    icolor ( ii ) = IMISSD
	    IF ( iptr ( ii ) .ne. IMISSD ) THEN
C
C*	 	Get the strike count for this minute.
C
		CALL LW_INPT ( lunf, iptr ( ii ), 1, nstrk, ier )
		IF ( nstrk .gt. LLMXPT ) nstrk = LLMXPT
C
C*	 	Get the remaining data for this minute.
C
		CALL LW_INPT ( lunf, iptr ( ii ) + 1, nstrk * 4, idata,
     +			       ier )
C
C*		Assign the color for this minute.
C
		CALL ST_INLN ( ii, cmin, lens, ier )
		IF ( lens .eq. 1 ) cmin = '0' // cmin ( :1 )
		datmin = datfil ( :9 ) // cmin
		CALL TI_MTCH ( 2, datmin, datgrp, ntimes, 0, iposn, 
     +			       ier )
		idx = ntimes - iposn
		IF ( ( idx .eq. 0 ) .and. 
     +		     ( datmin .le. dattm2 )  ) idx = 1
		IF ( datmin .eq. dattm2 ) THEN
		    lmin = ii
		  ELSE IF ( datmin .gt. dattm2 ) THEN
		    lmin = ii - 2
		END IF
		IF ( ( idx .gt. 0 ) .and. ( idx .lt. ntimes ) ) 
     +		       icolor ( ii ) = icolrs ( idx )
C
C*		Save the polarity and location for each strike.
C
		IF ( ii .le. lmin ) THEN
		    DO jj = 2, nstrk * 4, 4
			IF ( idata ( jj + 2 ) .gt. 0 ) THEN
			    IF ( ipos .lt. MAXSTK ) THEN
				ipos = ipos + 1
			        IF ( istrtp ( ii ) .eq. IMISSD ) 
     +				     istrtp ( ii ) = ipos
				rlat (ipos,1) = idata ( jj ) / 10000.
				rlon (ipos,1) = idata (jj+1) / (-10000.)
			    END IF
			  ELSE
			    IF ( ineg .lt. MAXSTK ) THEN
				ineg = ineg + 1
			        IF ( istrtn ( ii ) .eq. IMISSD )
     +				     istrtn ( ii ) = ineg
				rlat (ineg,2) = idata ( jj ) / 10000.
				rlon (ineg,2) = idata (jj+1) / (-10000.)
			    END IF
			END IF
		    END DO
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
	CALL FL_CLOS ( lunf, ier )
C
C*	Plot all data from this file.
C
	lmin1  = lmin + 1
	istrtp ( lmin1 ) = ipos + 1
	istrtn ( lmin1 ) = ineg + 1
	icolor ( lmin1 ) = -99
C
	icurnt = IMISSD
	ibpos  = IMISSD
	ibneg  = IMISSD
	iepos  = IMISSD
	ieneg  = IMISSD
C
	DO ii  = 0, lmin1
	    IF ( icolor ( ii ) .ne. IMISSD ) THEN
		IF ( icurnt .eq. IMISSD ) icurnt = icolor (ii) 
		IF ( ibpos .eq. IMISSD ) ibpos = istrtp (ii)
		IF ( ibneg .eq. IMISSD ) ibneg = istrtn (ii)
		IF ( icurnt .ne. icolor ( ii ) ) THEN
C
C*		    Find ending locations.
C
		    iepos = IMISSD
		    IF ( ibpos .ne. IMISSD ) THEN
			found = .false.
			jj    = ii
			DO WHILE ( ( .not. found ) .and.
     +				   ( jj .le. lmin1 ) )
			    IF ( istrtp (jj) .ne. IMISSD ) THEN
				iepos = istrtp ( jj )
				found = .true.
			    END IF
			    jj = jj + 1
			END DO
		    END IF
C
		    ieneg = IMISSD
		    IF ( ibneg .ne. IMISSD ) THEN
			found = .false.
			jj    = ii
			DO WHILE ( ( .not. found ) .and.
     +				   ( jj .le. lmin1 ) )
			    IF ( istrtn (jj) .ne. IMISSD ) THEN
			        ieneg = istrtn ( jj )
			        found = .true.
			    END IF
			    jj = jj + 1
			END DO
		    END IF
C
C*		    Set color and attributes, and plot the markers. 
C
		    CALL GSCOLR ( icurnt, ier )
		    IF ( iepos .gt. ibpos ) THEN
	                CALL GSMRKR ( mark (1), mark (2), szmrkr (1),
     +				      mark (3), ier )
			np = iepos - ibpos
C
C*		        Draw the positive markers.
C
			IF  ( iflgps .ne. 0 )  THEN
			    CALL GMARK ( 'M', np, rlat ( ibpos, 1 ),
     +				         rlon ( ibpos, 1 ), ier )
			END IF
		    END IF
C
		    IF ( ieneg .gt. ibneg ) THEN
	                CALL GSMRKR ( mark (4) , mark (5), szmrkr (2) ,
     +				      mark (6), ier )
			np = ieneg - ibneg
C
C*		        Draw the negative markers.
C
			IF  ( iflgng .ne. 0 )  THEN
			    CALL GMARK ( 'M', np, rlat ( ibneg, 2 ),
     +				         rlon ( ibneg, 2 ), ier )
			END IF
		    END IF
		    icurnt = icolor ( ii )
		    ibpos  = istrtp ( ii )
		    ibneg  = istrtn ( ii )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
