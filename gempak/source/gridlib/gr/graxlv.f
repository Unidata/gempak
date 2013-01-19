	SUBROUTINE GR_AXLV  ( dmin, dmax, start, stop, rint, stradj,
     +                        stpadj, v, nv, iret )
C************************************************************************
C* GR_AXLV								*
C*									*
C* This subroutine defines axis label values given the data range,	*
C* the axis range and labeLling interval, if it is defined.  A suitable	*
C* label interval is determined automatically if it is missing.		*
C*									*
C* GR_AXLV  ( DMIN, DMAX, START, STOP, RINT, STRADJ, STPADJ, V, NV,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DMIN		REAL		Minimum data value		*
C*	DMAX		REAL		Maximum data value		*
C*	START		REAL		Starting value on axis		*
C*	STOP		REAL		Stopping value on axis		*
C*	RINT		REAL		Labelling interval		*
C*	STRADJ		LOGICAL		Flag to permit adjusting START	*
C*	STPADJ		LOGICAL		Flag to permit adjusting STOP	*
C*									*
C* Output parameters:							*
C*	V  ( NV )	REAL		Array of label values		*
C*	NV		INTEGER		Number of label values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C*					-15 = START or STOP missing	*
C*					-17 = Scaling cannot be done	*
C**									*
C* Log:									*
C* K. Brill/GSC		 5/90        					*
C* K. Brill/NMC      	07/90		Checks for error conditions	*
C* J. Wu/GSC            07/00           Moved INCLUDE 'ERMISS.FNC'      *  
C*                                      before the DATA statement       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		v (*)
	LOGICAL  	stradj, stpadj
C*
	REAL    	qinc (5)
	LOGICAL 	notdon
	INCLUDE		'ERMISS.FNC'
	DATA  		qinc /1.,2.,3.,5.,10./
C*
C------------------------------------------------------------------------
	iret = 0
	nv = 0
C
C*	Check for errors in input.
C
	IF  ( ERMISS ( start ) .or. ERMISS ( stop ) .or.
     +        start .eq. stop ) THEN
	    iret = -15
	    RETURN
	END IF
	rmin = AMIN1 ( start, stop )
	rmax = AMAX1 ( start, stop )
C*
	IF ( .not. ERMISS ( rint ) ) THEN
C
C*	  If the interval is defined, use it to generate labels.
C
	  CALL GR_CMPV ( rmin, rmax, rint, LLAXIS, nv, v, ire )
C*
	ELSE
C
C*        Generate the label interval automatically.
C
	  delta = ABS ( start - stop )
	  ipwr = INT ( ALOG10 ( delta ) ) - 2
	  IF ( ipwr .lt. 0 ) ipwr = ipwr - 1
          notdon = .true.
	  DO WHILE ( notdon )
	    ipwr = ipwr + 1
	    IF ( ipwr .gt. 29 ) THEN
	       iret = -17
	       CALL ER_WMSG ( 'GR', iret, ' ', ier )
	       RETURN
	    END IF
            fctr = 10. ** ipwr 
	    IF ( ipwr .lt. 0 ) THEN
	      ipos = -ipwr
	      fctr = 1.0000001 / 10. ** ipos  
	    END IF
	    i = 0
            DO WHILE ( i .lt. 5 .and. notdon )
	      i = i + 1
	      aninc = qinc ( i ) * fctr
	      itest = INT ( delta/aninc )
	      IF ( itest .lt. 7 ) notdon = .false.
	    END DO
	  END DO
	  CALL GR_CMPV ( rmin, rmax, aninc, LLAXIS, nv, v, ire )
	END IF
C
C*	If DMIN or DMAX is missing, START and STOP cannot be adjusted.
C*      If there is no adjusting to be done, RETURN.
C
	IF ( ERMISS ( dmin ) .or. ERMISS ( dmax ) .or.
     +     ( .not. stradj .and. .not. stpadj ) )       RETURN
C*
	  test = (start - stop ) * ( dmin - dmax )
	IF ( stradj ) THEN
	  IF ( test .lt. 0.0 .and. 
     +       ( v (nv) .lt. start .and. v (nv) .gt. dmax ) )
     +           start = v (nv)
	  IF ( test .gt. 0.0 .and.
     +       ( v (1) .gt. start .and. v (1) .lt. dmin ) )
     +           start = v (1)
	END IF
C*
	IF ( stpadj ) THEN
	  IF ( test .lt. 0.0 .and.
     +       ( v (1) .lt. dmin .and. v (1) .gt. stop ) )
     +         stop = v (1)
	  IF ( test .gt. 0.0 .and.
     +       ( v (nv) .gt. dmax .and. v (nv) .lt. stop ) )
     +         stop = v (nv)
	END IF
C*
	RETURN
	END
