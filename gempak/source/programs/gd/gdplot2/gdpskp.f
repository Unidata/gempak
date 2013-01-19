	SUBROUTINE GDPSKP  ( skip, filter, iskpxy, ixinc, 
     +			iyinc, istag, latthn, alatsk, 
     +			lfilt, offset, iret )
C************************************************************************
C* GDPSKP								*
C*									*
C* This subroutine finds the grid points at which wind barbs or arrows	*
C* should be plotted.  The variable SKIP contains the increment for	*
C* x and y to be used in skipping grid points.				*
C*									*
C* GDPSKP  ( SKIP, FILTER, ISKPXY, IXINC, IYINC, ISTAG, LATTHN, ALATSK, *
C*	     LFILT, OFFSET, IRET )					*
C*									*
C* Input parameters:							*
C*	SKIP		CHAR*		Value for point input by user	*
C*	FILTER		CHAR*		Value for filtering 		*
C*									*
C* Output parameters:							*
C*	ISKPXY		INTEGER		Scaler skip increment		*
C*	IXINC		INTEGER		Increment in x dir (vector)	*
C*	IYINC		INTEGER		Increment in y dir (vector)	*
C*	ISTAG		INTEGER		Increment for stagger (vector)	*
C*	LATTHN		LOGICAL		Flag for lat thinning (vector)	*
C*	ALATSK(*)   	REAL 		Array for lat thinning (vector) *
C*	LFILT		LOGICAL		Flag for filtering		*
C*	OFFSET(*)	REAL 		Array for filter offsets	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
c*					 -9 = invalid grid subset area	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* K. Brill/NMC		 1/92	Replaced POINTS with SKIP		*
C* K. Brill/NMC		 2/92	Add 1 to ISKPWN				*
C* L. Sager/NMC		 6/93	Add stagger to skip increments		*
C* S. Jacobs/NCEP	 5/96	Added istag to calling sequence		*
C* D.W.Plummer/NCEP	11/96   Started w/ GDBPNT, added lat thinning	*
C* D.W.Plummer/NCEP	 4/97	Added filter processing			*
C* D.W.Plummer/NCEP	 7/98	Move CALL IN_SKIP to beginning		*
C* T. Lee/GSC		 8/99	Fixed latitudinal thinning indices	*
C* S. Jacobs/NCEP	12/06	Fixed variable in GTRANS call		*
C************************************************************************
	CHARACTER*(*)	skip, filter
	LOGICAL		latthn
	REAL		alatsk (*), offset (*)
	LOGICAL		lfilt
C* 
	INTEGER		iskpwn (2)
C------------------------------------------------------------------------
	iret = 0
	istag = 0
C*
	CALL IN_SKIP ( skip, iskpxy, iskpwn, ier )
C*
C*	Check for filter processing
C*
	CALL IN_FILT ( filter, filtfc, ier )
C
	CALL GQSYSZ  ( rwmrk, rhmrk, rwtxt, rhtxt, rwbrb, rhbrb, iret )
C
	DO ii = 1, 4
	    offset (ii) = rwbrb * 0.5 * filtfc
	END DO
C
	lfilt = .not. ( filtfc .eq. 0.0 )
C
	IF ( lfilt )  THEN
	    ixinc = 1
	    iyinc = 1
	    RETURN
	END IF
C*
C*	Break up skip parameter
C
	ix1   = 1
	iy1   = 1
C
	ixinc = iskpwn ( 1 )
	iyinc = iskpwn ( 2 )
C
	IF  ( iyinc .ge. 0 )  THEN
C
C*	    Normal skip processing
C
	    latthn = .false.
C
C*	    Check for stagger
C
	    IF ( ixinc .ge. 0 )  THEN
		ixstep = ixinc + 1
		istag = 0
	      ELSE
		ixstep = - ixinc + 1
		istag = ixstep / 2
	    END IF
	    iystep = iyinc + 1
C
	  ELSE IF ( iyinc .lt. 0 )  THEN
C
C*          Latitudinal thinning processing ( iyinc .lt. 0 ).
C*
C*          For each latitude degree, figure out the number of
C*          wind barbs that would fit around a latitude circle,
C*          given the currect size of a wind barb (rszwb) and the
C*          graphical distance along the latitude line (dd).
C*          The variable ixinc is not used.
C
	    latthn = .true.
	    istag = 0
	    ixstep = 1
	    iystep = - iyinc
C
	    CALL GQSYSZ ( rxszmk, ryxzmk, rxsztx, rysztx,
     +                    rxszwb, ryszwb, iret )
            rszwb = SQRT ( rxszwb*rxszwb + ryszwb*ryszwb )
            CALL GQBND ( 'M', rlatmn, rlonmn,
     +                   dlatmx, dlonmx, iret )
C
C*          Loop through each latitude.
C
            DO  ialt = -90, 90
	    	alt  = FLOAT ( ialt )
                alt1 = alt + 91.0
                CALL GTRANS( 'M', 'N', 1, alt, rlonmn,
     +                       xout1, yout1, iret )
                CALL GTRANS( 'M', 'N', 1, alt, rlonmn+1,
     +                       xout2, yout2, iret )
                dd = SQRT ( (xout1-xout2)**2 + (yout1-yout2)**2 )
                dd = dd * 360.0
                alatsk( INT (alt1) ) = dd / rszwb
            END DO
C
C*          Set north and south pole values such that only
C*          one wind barb gets plotted there.
C
            alatsk(  1) = 0.0
            alatsk(181) = 0.0
C
	END IF
C
	ixinc = ixstep
	iyinc = iystep
C*
	RETURN
	END
