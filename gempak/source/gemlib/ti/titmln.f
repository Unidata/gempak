	SUBROUTINE TI_TMLN ( timin, nin, mrange, intrvl, idir, iflag, 
     +			     iauto, basetm, endtim, idelrt, timout, 
     +			     ntime, iret )
C************************************************************************
C* TI_TMLN								*
C*									*
C* This subroutine return the time steps for time line based on input	*
C* time and interval.  If the interval is 0 or negative, all time	*
C* steps will be returned.						*
C*									*
C* TI_TMLN ( TIMIN, NIN, MRANGE, INTRVL, IDIR, IFLAG, IAUTO, BASETM,	*
C*		ENDTIM, IDELRT, TIMOUT, NTIME, IRET )			*
C*									*
C* Input parameters:							*
C*	TIMIN		CHAR*		Date/time			*
C*	NIN		INTEGER		Number of time			*
C*	MRANGE		INTEGER		Time range in minutes		*
C*	INTRVL		INTEGER		Time interval in minutes	*
C*	IDIR		INTEGER		Time line direction		*
C*					 -1: backward/image data	*
C*					  0: backward			*
C*					  1: forward			*
C*	IFLAG		INTEGER		Reference time flag		*
C*					 -1: for data loading		*
C*					  0: no reference time		*
C*					  1: reference time used	*
C*	IAUTO		INTEGER		Auto update flag		*
C*					  0: no auto update		*
C*					  1: auto update		*
C*	BASETM		CHAR*		Base time			*
C*	ENDTIM		CHAR*		End time, could be ref. time	*
C*					  or system time		*
C*									*
C* Input and Output parameters:						*
C*	IDELRT		INTEGER		Delta reference time in minutes	*
C*									*
C* Output parameters:							*
C*	TIMOUT		CHAR*		Date/time			*
C*	NTIME		INTEGER		Number of time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = no time in data set	*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/03						*
C* T. Lee/SAIC		12/03	Accommodated reference time		*
C* T. Lee/SAIC		02/04	Added auto-update			*
C* T. Lee/SAIC		04/04	Added delta reference time		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   timin (*), timout (*), endtim, basetm
	CHARACTER	tms*12
	LOGICAL		done
	INTEGER		itarr (5), jtarr (5)
	PARAMETER	(HALF_D = 720, ONE_H = 60 )
C------------------------------------------------------------------------
	iret = 0
C
	IF  ( nin .eq. 0 )  THEN
	    timout (1) = ' '
	    ntime = 0	
	    iret = -5
	    RETURN
	  ELSE IF ( nin .eq. 1 )  THEN
	    timout (1) = timin  (1)
	    ntime = 1
	    RETURN
	END IF
C
C*	Get right sorting order. Obs. and analysis data are in 
C*	descending order.  Forecast data are in ascending order.
C
	CALL TI_DIFF ( timin (2), timin (1), idt, iret )
	IF  ( idt .eq. 0 )  THEN
	    timout (1) = timin (1)
	    ntime = 1
	    RETURN	
	  ELSE IF  ( idt .gt. 0 )  THEN
C
C*	    If data time is in ascending order, resequence to
C*	    descending order for obs., image, and analysis data.
C
	    IF  ( idir .le. 0 )  THEN
		CALL TI_RSEQ  ( nin, timin, timin, iret )
	    END IF	
	  ELSE
C
C*	    For forecast grid, the data is in ascending order.
C	
	    IF  ( idir .eq. 1 )  THEN 
		CALL TI_RSEQ  ( nin, timin, timin, iret )
	    END IF
	    idt = - idt
	END IF
C
C*	If iflag less than zero, return all data for data loading
C*	and time matching
C
	IF ( iflag .lt. 0 )  THEN
	    DO i = 1, nin
		timout ( i ) = timin ( i )
	    END DO
	    ntime = nin
	    RETURN
	END IF
C
C*	If delta reference time is not blank, get new endtime and set 
C*	iflag to 1 to facilitate calculation.
C
	IF  ( idelrt .gt. 0 .and. iflag .eq. 0 )  THEN
	    CALL TI_CTOI ( timin ( 1 ), itarr, ier )
	    IF  ( idir .le. 0 )  THEN
		CALL TI_SUBM ( itarr, idelrt, jtarr, ier )  
	      ELSE
		CALL TI_ADDM ( itarr, idelrt, jtarr, ier )
	    END IF
	    CALL TI_ITOC ( jtarr, endtim, ier )
	    iflag  = 1
	END IF
C
C*	If reference time is used, use endtime to compute data range.
C*	If time range (MRANGE) is greater than data range, the latter 
C*	will be used for time range.
C
	IF  ( iflag .eq. 1 )  THEN
	    IF  ( idir .le. 0 )  THEN
		CALL TI_DIFF ( endtim, timin (nin), irange, ier )
		IF  ( irange .lt. 0 )  THEN
		    RETURN
		  ELSE
		    CALL TI_DIFF ( timin (1), endtim, idelrt, ier )
		END IF
	      ELSE
		CALL TI_DIFF ( timin (nin), endtim, irange, ier )
		IF  ( irange .lt. 0 )  THEN
	    	    timout (1) = ' '
	    	    ntime = 0	
	    	    iret = -5
		    RETURN
		  ELSE
		    CALL TI_DIFF ( endtim, timin (1), idelrt, ier )
		END IF
	    END IF
	  ELSE
            CALL TI_DIFF ( timin (nin), timin (1), irange, iret )
	END IF
C
	IF  ( ( mrange .gt. ABS (irange) ) .or. ( mrange .le. 0 ) )
     +		mrange = ABS ( irange )
C
	ntime = 0
	IF  ( intrvl .gt. 0 ) nmarks = mrange / intrvl + 1
        done = .false.
	IF  ( intrvl .le. 0 )  THEN
C
	  i = 1
C
	  IF  ( iauto .gt. 0  )  THEN
C
C*	    Auto-update: ON (image data only)
C*	    Intervals: OFF
C*	    Reference time: OFF
C
	    DO WHILE  ( .not. done )
	      CALL TI_DIFF ( timin ( 1 ), timin ( i ), jrange, ier )
	      jrange = ABS ( jrange )
	      IF  ( ( ier .eq. 0 ) .and. ( jrange .le. mrange ) ) THEN
		ntime = ntime + 1
		timout ( ntime ) = timin  ( i )
	      ELSE 
		done = .true.
	      END IF
	      i = i + 1
	      IF  ( ( i .gt. nin ) .or. ( ntime .ge. MXNMFL ) ) 
     +			done = .true.
	    END DO
	  ELSE
C
C*	    Auto-update: OFF
C*	    Intervals: OFF
C*	    Reference time: ON
C
	    IF ( iflag .eq. 1 )  THEN
	      DO WHILE  ( .not. done )
		IF  ( idir .ge. 1 )  THEN
		  CALL TI_DIFF ( timin ( i ), endtim, jrange, ier )
		ELSE
		  CALL TI_DIFF ( endtim, timin ( i ), jrange, ier )
		END IF
		IF  ( ( ier .ne. 0 ) .or. ( jrange .gt. mrange ) ) THEN
		  done = .true.
		ELSE
		  IF ( jrange .ge. 0 )  THEN
		    ntime = ntime + 1
		    timout ( ntime ) = timin  ( i )
		  END IF
		END IF
		i = i + 1
	        IF  ( ( i .gt. nin ) .or. ( ntime .ge. MXNMFL ) ) 
     +			done = .true.
	      END DO
	    ELSE
C
C*	      Auto-update: OFF
C*	      Intervals: OFF
C*	      Reference time: OFF
C
	      DO WHILE  ( .not. done )
	        CALL TI_DIFF ( timin ( 1 ), timin ( i ), jrange, ier )
		jrange = ABS ( jrange )
		IF  ( ( ier .eq. 0 ) .and. ( jrange .le. mrange ) )  THEN
		  ntime = ntime + 1
		  timout ( ntime ) = timin  ( i )
		ELSE 
		  done = .true.
		END IF
		i = i + 1
	        IF  ( ( i .gt. nin ) .or. ( ntime .ge. MXNMFL ) ) 
     +			done = .true.
	      END DO
C
	    END IF
C
	  END IF
C	     
	ELSE
C
C*	  Auto-update: ON/OFF
C*	  Intervals: ON
C*	  Reference time: OFF
C*	  (for image data only: if auto update is on or get correct time line 
C*	   when users turn the auto-update button off then turn it back on).
C
	  IF  ( ( idir .lt. 0 ) .and. ( iflag .eq. 0 ) )  THEN
	    IF  ( basetm .ne. ' ' )  THEN
	        CALL TI_CTOI ( basetm, itarr, ier )
	        CALL TI_DIFF ( timin (1), basetm, nmin, ier )  
C
	        IF  ( nmin .ge. intrvl )  THEN
	          nmin = ( nmin / intrvl ) * intrvl
	          CALL TI_ADDM ( itarr, nmin, jtarr, ier )
	          CALL TI_ITOC ( jtarr, basetm, ier )	
	          CALL TI_CTOI ( basetm, itarr, ier )	
	        END IF
	        tms = basetm
	      ELSE
	        tms = timin (1)
		CALL TI_CTOI ( tms, itarr, ier )
	    END IF 
	    DO WHILE  ( .not. done )
		ntime = ntime + 1
		timout ( ntime ) = tms
		CALL TI_SUBM ( itarr, intrvl, jtarr, ier )
		CALL TI_ITOC ( jtarr, tms, ier )
		DO j = 1, 5
		  itarr ( j ) = jtarr ( j )
		END DO
		nmarks = nmarks - 1
		IF  ( ( nmarks .le. 0 ) .or. ( ntime .ge. MXNMFL ) )
     +			done = .true.
	    END DO
C
	  ELSE
C
C*	    Auto-update: OFF
C*	    Intervals: ON
C*	    Reference time: ON
C
	    IF ( iflag .eq. 1 )  THEN
	      tms = endtim
	    ELSE
C
C*	      Auto-update: OFF
C*	      Intervals: ON
C*	      Reference time: OFF
C
	      tms = timin ( 1 )
	    END IF
	    CALL TI_CTOI ( tms, itarr, ier )
	    DO WHILE ( .not. done )
		ntime = ntime + 1
		timout ( ntime ) = tms
		IF  ( idir .le. 0 )  THEN
		  CALL TI_SUBM ( itarr, intrvl, jtarr, ier ) 
		ELSE
		  CALL TI_ADDM ( itarr, intrvl, jtarr, ier )
		END IF
		CALL TI_ITOC ( jtarr, tms, ier )
		nmarks = nmarks - 1
		IF ( ( nmarks .le. 0 ) .or. ( ntime .ge. MXNMFL ) ) THEN
     			done = .true.
		ELSE
		  DO j = 1, 5
		    itarr ( j ) = jtarr ( j )
		  END DO
		END IF
	    END DO
C
	  END IF
	END IF
C*
	RETURN
	END
