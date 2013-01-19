	SUBROUTINE GR_MBN2  ( deltan, iebnds, dbnds, rnvblk, anlblk,
     +			      iret )
C************************************************************************
C* GR_MBN2								*
C*									*
C* This subroutine makes a general Barnes analysis block.  The analysis	*
C* block created is LLNANL words long.  All the bounds must be entered	*
C* in the order:  lower left latitude; lower left longitude; upper	*
C* right latitude; upper right longitude.				*
C*									*
C* GR_MBN2  ( DELTAN, IEBNDS, DBNDS, RNVBLK, ANLBLK, IRET )		*
C*									*
C* Input parameters:							*
C*	DELTAN		REAL		Station spacing			*
C*	IEBNDS (4)	INTEGER		Extended bounds in grid units	*
C*	DBNDS  (4)	REAL		Data area bounds		*
C*	RNVBLK (LLNNAV)	REAL		Navigation block		*
C*									*
C* Output parameters:							*
C*	ANLBLK (LLNANL)	REAL		Analysis block			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = invalid navigation	*
C**									*
C* Log:									*
C* M. desJardins/NMC	 4/91						*
C* K. Brill/NMC		05/91	Add rather than subtract IEXTND to KX,KY*
C* 				Corrected input lat/lon in GSMPRJ for	*
C*				the extend region. Change IER1 to IER in*
C*				summing rc's from setting extend rgn.	*
C* K. Brill/NMC		02/92	Use LLNNAV and LLNANL in documentation	*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		dbnds (*), rnvblk (*),  anlblk (*)
	INTEGER		iebnds (*)
C*
	REAL		rx (2), ry (2), rlatm (2), rlonm (2), ebnds (4),
     +			gbnds (4)
	INTEGER		iextnd (4)
	CHARACTER	proj*4
	LOGICAL		dflag, eflag
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	The first word in the analysis block is the analysis type.
C
	anlblk (1) = 2.0
C
C*	Set up navigation in GEMPLT.
C
	CALL GR_SNAV ( 13, rnvblk, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -6
	    CALL ER_WMSG ( 'GDCFIL', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Get the projection name and the number of grid points.
C*	Also, get the grid bounds.
C
	CALL GR_RNAV  ( rnvblk, proj, kx, ky, ier )
C
C*	Get the range of data to accept for the grid area.
C*	First, set up the area as a map projection and then
C*	query the bounds.
C
	CALL GQGPRJ  ( proj, a1, a2, a3, kx, ky, gbnds (1),
     +		       gbnds (2), gbnds (3), gbnds (4), ier2 )
	CALL GSMPRJ  ( proj, a1, a2, a3, gbnds (1), gbnds (2),
     +		       gbnds (3), gbnds (4), ier )
	CALL GQBND   ( 'M', gbnds (1), gbnds (2), gbnds (3),
     +			gbnds (4), ier4 )
C
C*	CHECK FOR A VALID deltan.  IF non-positive, compute a default
C*	assuming that the grid point spacing approximates half the
C*	average station spacing.  Get the grid spacing at the center
C*	of the grid.
C
	IF ( deltan .gt. 0.0 ) THEN
	    anlblk (2) = deltan
	  ELSE
	    rx (1) = kx / 2
	    rx (2) = rx (1) + 1.
	    ry (1) = ky / 2
	    ry (2) = ry (1) + 1.
	    CALL GTRANS  ( 'G', 'M', 2, rx, ry, rlatm, rlonm, ier )
	    dellon = ( rlonm (2) - rlonm (1) ) 
	    dellat = ( rlatm (2) - rlatm (1) )
	    rl = dellon ** 2 + dellat ** 2
	    deln = SQRT ( rl / 2. )
	    anlblk (2) = deln * 2.
	END IF   
C
C*	Check the extend region.
C
	eflag = .false.
	DO  i = 1, 4
	    IF  ( iebnds (i) .le. 0 )  THEN
		iextnd (i) = 0
	      ELSE
		iextnd (i) = iebnds (i)
		eflag = .true.
	    END IF
	END DO
C
C*	Determine grid, extend, and data areas.
C
	IF  ( eflag )  THEN
	    rx (1) = 1. - iextnd (1)
	    ry (1) = 1. - iextnd (2)
	    rx (2) = kx + iextnd (3)
	    ry (2) = ky + iextnd (4)
C
C*	    Translate the extended grid coordinates to lat/lon.
C*	    Then, set up a projection using these coordinates.
C*	    Check for a valid projection.
C
	    CALL GTRANS  ( 'G', 'M', 2, rx, ry, rlatm, rlonm,
     +			   ier )
	    CALL GQGPRJ  ( proj, a1, a2, a3, kx, ky, ebnds (1),
     +			   ebnds (2), ebnds (3), ebnds (4), ier2 )
	    CALL GSMPRJ  ( proj, a1, a2, a3, rlatm (1), rlonm (1),
     +			   rlatm (2), rlonm (2), ier3 )
	    CALL GQBND   ( 'M', ebnds (1), ebnds (2), ebnds (3),
     +			    ebnds (4), ier4 )
	    ier = ier + ier2 + ier3 + ier4	
C
C*	    Check for valid operations.
C
	    IF ( ier .ne. 0 ) THEN
		ier = +4
		CALL ER_WMSG  ( 'GR', ier, ' ', ier1 )
		DO  i = 1, 4
		    iextnd (i) = 0
		END DO
		eflag = .false.
	    END IF
	END IF
C
C*	Check for invalid CED coordinates.
C
	IF  ( ( proj .eq. 'CED' ) .and. ( eflag ) )  THEN
	    dlon = rnvblk (10) - rnvblk (8)
	    IF  ( dlon .le. 0. )  dlon = 360. + dlon
	    dellon = dlon / ( kx - 1 )
	    dlon = dlon + dellon * ( iextnd (1) + iextnd (3) )
	    IF  ( dlon .gt. 360. )  THEN
		ier = +4
		CALL ER_WMSG  ( 'GR', ier, ' ', ier1 )
		DO  i = 1, 4
		    iextnd (i) = 0
		END DO
		eflag = .false.
	    END IF
	END IF
C
C*	Add the extend bounds.
C
	DO  i = 1, 4
	    anlblk (i+2) = iextnd (i)
	END DO
C
C*	Add the grid area.
C
	DO  i = 1, 4
	    anlblk (i+6) = gbnds (i)
	END DO
C
C*	Add the extended area.
C
	IF  ( eflag )  THEN
	    DO  i = 1, 4
		anlblk (i+10) = ebnds (i)
	    END DO
	  ELSE
	    DO  i = 1, 4
		anlblk (i+10) = gbnds (i)
	    END DO
	END IF
C
C*	Check to see if there are valid data area bounds.
C*	Add them to the analysis block.
C
	dflag = .false.
	DO  i = 1, 4
	    IF  ( ( .not. ERMISS ( dbnds (i) ) ) .or.
     +		  ( dbnds (i) .eq. 0. ) )  dflag = .true.
	END DO
	DO  i = 1, 4
	    IF  ( dflag )  THEN
		anlblk (i+14) = dbnds (i)
	      ELSE
		anlblk (i+14) = anlblk (i+10)
	    END IF
	END DO
C*
	RETURN
	END
