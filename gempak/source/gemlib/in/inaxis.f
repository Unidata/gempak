	SUBROUTINE IN_AXIS  ( axis, ivcrd, skewt, parm, dmin, dmax,
     +                        ilfdef, igfdef, itfdef, start, stop,
     +                        values, nval, ilbfrq, iglfrq, itmfrq,
     +                        iret )
C************************************************************************
C* IN_AXIS								*
C*									*
C* This subroutine processes an axis variable.  The start and stop	*
C* values along with an array of values are returned.  The frequencies	*
C* with respect to the elements in the array of values for the		*
C* plotting of labels, grid lines and tick marks are also returned.	*
C* If any of these frequency values is missing, the corresponding	*
C* output value is set to the input defaults.  Plotting begins with	*
C* the first element.							*
C*									*
C* AXIS is expected to be of the form:					*
C*									*
C*	    start/stop/increment/labfrq;glnfrq;ticfrq			*
C*    or								*
C*	    start/stop/value1;value2;...;valueN/labfrq;glnfrq;ticfrq	*
C*									*
C* In the latter case, the increment specification has been replaced	*
C* with a list of values.  Failure to specify START and STOP will	*
C* result in default values determined on the basis of DMIN and DMAX,	*
C* the vertical coordinate or the parameter.  If increment = MAN,	*
C* then the mandatory levels between START and STOP are returned.	*
C* A positive increment will generate values divisible by the		*
C* increment.  A negative increment will generate values incremented	*
C* from START using the absolute value of the increment.  If the	*
C* SKEWT flag is set, extra lines will be added on the lower end	*
C* of the scale.							*
C*									*
C* NOTE:  Dimension VALUES to LLAXIS in the calling program.		*
C*									*
C* IN_AXIS  ( AXIS, IVCRD, SKEWT, PARM, DMIN, DMAX, ILFDEF, IGFDEF,	*
C*	      ITFDEF, START, STOP, VALUES, NVAL, ILBFRQ, IGLFRQ,	*
C*	      ITMFRQ, IRET )						*
C*									*
C* Input parameters:							*
C*	AXIS		CHAR*		Input for axis			*
C*	IVCRD		INTEGER		Vertical coordinate		*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*					  4 = SGMA			*
C*					  5 = DPTH			*
C*					  6 = HYBL			*
C*	SKEWT		LOGICAL		Flag skewT plot T axis		*
C*	PARM		CHAR*		Parameter name (optional)	*
C*	DMIN		REAL		Data minimum			*
C*	DMAX		REAL		Data maximum			*
C*	ILFDEF		INTEGER		Default label frequency		*
C*	IGFDEF		INTEGER		Default grid line frequency	*
C*	ITFDEF		INTEGER		Default tick mark frequency	*
C*									*
C* Output parameters:							*
C*	START		REAL		Starting value for axis		*
C*	STOP		REAL		Stopping value for axis		*
C*	VALUES (NVAL)	REAL		Array of values			*
C*	NVAL		INTEGER		Number of values		*
C*	ILBFRQ		INTEGER		Label frequency			*
C*	IGLFRQ		INTEGER		Grid line frequency		*
C*	ITMFRQ		INTEGER		Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = incorrect specification	*
C*					 -5 = MAN lvls not appropriate	*
C*					 -6 = axis bounds are missing	*
C**									*
C* Log:									*
C* K. Brill/GSC		05/90						*
C* S. Schotz/GSC	 7/90	Added default values as inputs for	*
C*				frequencies				*
C* S. Jacobs/SSAI	10/91	Changed number of extra lines, when	*
C*				plotting a skewt, to 30			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Lee/GSC		 2/98	Labeled 925 mb on vertical axis		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* T. Lee/SAIC		12/01	Increased substr string length to LLMXLN*
C* F. J. Yen/NCEP	 7/08	Added start, stop, & rint defaults for	*
C*				HYBL(ivcrd=6). Add SGMA & DPTH to prolog*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	axis, parm
	LOGICAL         skewt
	REAL            values (*)
C*
	CHARACTER	substr (4) * (LLMXLN)
	INTEGER         ihold (3)
	LOGICAL		useprm, stradj, stpadj
C*
	PARAMETER	( NP = 16 )
	REAL		plbl (NP)
	REAL		xbegin (8), xend (8), xdif (8)
        INCLUDE         'ERMISS.FNC'
	DATA		xbegin  / -80., -110., 200., 250., -60., -80.,
     +				  230., 250. /
	DATA		xend    /  40.,  110., 320., 400.,  40., 110.,
     +				  320., 450. /
	DATA		xdif    /  20.,  40.,  20.,   50.,  20.,  40.,
     +				   20.,  50. /
	DATA		plbl / 1000., 925., 850., 700., 500., 400., 300.,
     +				 250., 200., 150., 100., 70., 50.,
     +				  30.,  20.,  10. /
C*
C------------------------------------------------------------------------
	iret = 0
	rint = RMISSD
	stradj = .false.
	stpadj = .false.
	useprm = .false.
C
C*	Set DMIN and DMAX under appropriate circumstances.
C
	IF ( dmin .eq. dmax .and. .not. ERMISS ( dmax ) ) THEN
	  IF ( dmin .eq. 0. ) THEN
	    dmin = -.5
	    dmax =  .5
	  ELSE
	    test = abs ( dmin )
	    ipow = ALOG10 ( test ) - 1
	    add  = .5 * ( 10. ** ipow )
	    dmax = dmax + add
	    dmin = dmin - add
	  END IF
	END IF
C
C*      Break AXIS into four substrings.
C
	CALL ST_CLST ( axis, '/', ' ', 4, substr, nst, ier )
	IF ( ier .ne. 0 ) THEN
	  iret = -2
	  CALL ER_WMSG ( 'IN', iret, axis, ier )
	  RETURN
	END IF
C
C*	Break the fourth substring into 3 frequency integers.  If the
C*      values are missing, set them to the input default values.
C
	CALL ST_ILST ( substr (4), ';', IMISSD, 3, ihold, nm, ier )
	DO i = 1, 3
	  IF ( ihold (i) .lt. 0 ) ihold (i) = IMISSD
	END DO
	IF  ( ihold (1) .ne. IMISSD )	THEN
            ilbfrq = ihold (1)
	ELSE
	    ilbfrq = ilfdef
        END IF
	IF  ( ihold (2) .ne. IMISSD )   THEN
            iglfrq = ihold (2)
        ELSE
            iglfrq = igfdef
	END IF
        IF  ( ihold (3) .ne. IMISSD )	THEN
            itmfrq = ihold (3)
        ELSE
	    itmfrq = itfdef
        END IF
C
C*      Decode START and STOP from first and second substrings.
C
	CALL ST_CRNM ( substr (1), start, ier )
	CALL ST_CRNM ( substr (2), stop,  ier )
C
C*	IF START or STOP is missing, set a flag to use PARM and
C*      determine index pointer for start, stop, and increment.
C
	IF ( ( ERMISS ( start ) .or. ERMISS ( stop ) ) .and.
     +         parm .ne. ' ' .and. ivcrd .eq. 0 )       THEN
C*
	  start = RMISSD
	  stop  = RMISSD
C
C*	  Execute case statement to assign index pointer to special
C*        default values.
C
	  index = 0
	  IF ( parm .eq. 'TEMP' .or. parm .eq. 'TMPC' .or.
     +         parm .eq. 'DWPT' .or. parm .eq. 'DWPC' ) THEN
            index = 1
	  ELSE IF ( parm .eq. 'TMPF' .or. parm .eq. 'DWPF' ) THEN
	    index = 2
          ELSE IF ( parm .eq. 'TMPK' .or. parm .eq. 'DWPK' ) THEN
	    index = 3
	  ELSE IF ( parm .eq. 'THTA' .or. parm .eq. 'THTE' ) THEN
	    index = 4
	  END IF
          IF ( index .gt. 0 .and. skewt ) index = index + 4
	  IF ( index .gt. 0 ) THEN
	    useprm = .true.
	    start = xbegin ( index )
	    stop  = xend ( index )
	    rint  = xdif ( index )
	  END IF
C*
	ELSE
	  useprm = .false.
	END IF
C
C*      If START and STOP are still missing, establish defaults.
C
	IF ( ERMISS ( start ) ) THEN
	  IF ( ivcrd .eq. 1 ) THEN
	    start = 1020.
	  ELSE IF ( ivcrd .eq. 2 ) THEN
	    start = 270.
	  ELSE IF ( ivcrd .eq. 3 ) THEN
	    start = 0.0
	  ELSE IF ( ivcrd .eq. 6 ) THEN
	    start = 60.
	  ELSE IF ( .not. useprm ) THEN
            IF ( ERMISS ( dmin ) .or. ERMISS ( dmax ) ) THEN
	      iret = -6
	      CALL ER_WMSG ( 'IN', iret, ' ', ier )
	      RETURN
	    ELSE
	      start = dmin - .0375 * ABS ( dmax - dmin )
C
C*	      Set flag telling that START is adjustable.
C
	      stradj = .true.
	    END IF
	  END IF
	END IF
	IF ( ERMISS ( stop ) ) THEN
          IF ( ivcrd .eq. 1 ) THEN
	    stop = 100.
	  ELSE IF ( ivcrd .eq. 2 ) THEN
	    stop = 400.
	  ELSE IF ( ivcrd .eq. 3 ) THEN
            stop = 20000.
	  ELSE IF ( ivcrd .eq. 6 ) THEN
	    stop = 1.
	  ELSE IF ( .not. useprm ) THEN
	    IF ( ERMISS ( dmax ) .or. ERMISS ( dmin ) ) THEN
	      iret = -6
	      CALL ER_WMSG ( 'IN', iret, ' ', ier )
	      RETURN
	    ELSE
	      stop = dmax + .0375 * ABS ( dmax - dmin )
C
C*            Set flag telling that STOP is adjustable.
C
	      stpadj = .true.
            END IF
	  END IF
	END IF
C
C*	At this point START and STOP are defined and not missing.
C
C*      Look at the third substring to see if it contains a list of
C*      values, a single number giving the increment or a request for
C*      mandatory levels.
C
	CALL ST_LCUC ( substr (3), substr (3), ier )
	IF ( substr (3) (1:3) .ne. 'MAN' ) THEN
	  CALL ST_RLST ( substr (3), ';', RMISSD, 20,
     +                   values, nval, ier )
	  IF ( .not. ERMISS ( values (1) ) ) rint = values (1)
	  IF ( nval .eq. 0 .or. ERMISS ( rint ) ) THEN
	    IF ( ivcrd .eq. 1 ) THEN
              substr (3) (1:3) = 'MAN'
	    ELSE IF ( ivcrd .eq. 2 ) THEN
	      rint = 10.
	    ELSE IF ( ivcrd .eq. 3 ) THEN
	      rint = 1000.
	    ELSE IF ( ivcrd .eq. 6 ) THEN
	      rint = 10.
	    ELSE
C
C*	      Do nothing -- allow rint to remain missing.
C
	    END IF
	  END IF
	END IF
C*
	IF ( substr (3) (1:3) .eq. 'MAN' ) THEN
C
C*	  Mandatory levels have been requested.
C
	  IF ( ivcrd .ne. 1 .and. parm .ne. 'PRES' ) THEN
            iret = -5
	    CALL ER_WMSG ( 'IN', iret, ' ', ier )
	    RETURN
	  END IF
C
C*	  Return only the mandatory levels between START and STOP.
C
	  rmin = AMIN1 ( start, stop )
	  rmax = AMAX1 ( start, stop )
	  nval = 0
	  DO i = 1, NP
	    IF ( plbl (i) .ge. rmin .and. plbl (i) .le. rmax ) THEN
	      nval = nval + 1
	      values ( nval ) = plbl (i)
	    END IF
	  END DO
	  IF ( nval .eq. 0 ) THEN
	    iret = -2
	    CALL ER_WMSG ( 'IN', iret, axis, ier )
	  END IF
	  RETURN
C*
	ELSE
C
C*	  Generate axis labels.
C
C*	  If NVAL is not 1, do nothing; otherwise, generate the values
C*        using the start value, the increment and the stop value.
C
	  IF ( nval .gt. 1 ) THEN
C
C*          Do nothing.
C
	  ELSE IF ( values (1) .eq. 0.0 ) THEN
	    nval = 2
	    values (1) = start
	    values (2) = stop
C*
	  ELSE IF ( values (1) .gt. 0 .or. ERMISS ( values (1) ) ) THEN
C
C*	    Set values that are divisible by the interval.
C
	    CALL GR_AXLV ( dmin, dmax, start, stop, rint, stradj,
     +			   stpadj, values, nval, iret )
	    IF ( iret .ne. 0 ) THEN
	      iret = -2
	      CALL ER_WMSG ( 'IN', iret, axis, ier )
	      RETURN
	    END IF
C*
	  ELSE IF ( values (1) .lt. 0 ) THEN
C
C*	    Set values by incrementing from START.
C
	    dq = -values (1)
	    IF ( start .gt. stop .and. dq .gt. 0.0 ) dq = -dq
	    diff = ABS ( stop - start )
	    values (1) = start
	    nval = 1
	    chck = ABS ( values (nval) + dq - start )
	    DO WHILE ( nval .lt. LLAXIS .and. chck .le. diff )
	      nval = nval + 1
	      values ( nval ) = values ( nval - 1 ) + dq
	      chck = ABS ( values (nval) + dq - start )
	    END DO
	  END IF
C*
	END IF
C
C*	    Add lines if this is a SKEWT plot.
C
	IF  ( ( skewt ) .and. ( nval .ge. 2 ) )  THEN
		rinc = values (2) - values (1)
C
C*		Add 30 lines; this number is arbitrary.
C
		IF  ( nval+30 .le. LLAXIS )  THEN
		    iadd = 30
		  ELSE
		    iadd = LLAXIS - nval
		END IF
		DO  i = nval, 1, -1
		    values ( i+iadd ) = values ( i )
		END DO
		DO  i = iadd, 1, -1
		    values ( i ) = values ( i+1 ) - rinc
		END DO
		nval = nval + iadd
	END IF
	RETURN
	END
