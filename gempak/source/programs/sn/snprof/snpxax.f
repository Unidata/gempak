	SUBROUTINE SNPXAX  ( pname1, pname2, xaxis, iyaxis, xmin, xmax,
     +			     xstrt, xstop, xlbl, nxlbl, ixlbfr, ixglfr,
     +                       ixtmfr, iret )
C************************************************************************
C* SNPXAX								*
C*									*
C* This subroutine defines the x axis to be drawn in SNPROF.		*
C*									*
C* SNPXAX  ( PNAME1, PNAME2, XAXIS, IYAXIS, XMIN, XMAX, XSTRT, XSTOP,	*
C*           XLBL, NXLBL, IXLBFR, IXGLFR, IXTMFR, IRET )		*
C*									*
C* Input parameters:							*
C*	PNAME1		CHAR*		Parameter 1			*
C*	PNAME2		CHAR*		Parameter 2			*
C*	XAXIS		CHAR*		X axis input			*
C*	IYAXIS		INTEGER		Y axis coordinate		*
C*      XMIN            REAL		Minimum x value			*
C*      XMAX        	REAL		Maximum x value			*
C*									*
C* Output parameters:							*
C*	XSTRT		REAL		Left x				*
C*	XSTOP		REAL		Right x				*
C*	XLBL  (NXLBL)	REAL		X labels			*
C*	NXLBL		INTEGER		Number of x labels		*
C*      IXLBFR          INTEGER         Label frequency			*
C*      IXGLFR          INTEGER         Grid line frequency		*
C*      IXTMFR          INTEGER         Tick mark frequency             *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                       -5 = need range for axis	*
C*					 -6 = x coords not identical	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 4/89	Check for XAXIS separated with ;	*
C* K. Brill/GSC          5/90   Changes for IN_AXIS			*
C* S. Schotz/GSC	 7/90	Update for new IN_AXIS calling sequence	*
C* S. Schotz/GSC	 8/90	Added call to GEPLOT for error write	*
C* K. Brill/NMC		 1/91   Set default grid line freq to zero	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	pname1, pname2, xaxis
	REAL		xlbl (*)
C*
	LOGICAL         skew
C----------------------------------------------------------------------
	iret = 0
	ivc = 0
	skew = .false.
	IF  ( iyaxis .eq. 4 )  skew = .true.
C
C*	Check for unit consistency if two temperatures are requested.
C*	Type  0 = none,  1 = degrees C,  2 = degrees F,  3 = K.
C
	IF  ( pname1 .eq. ' ' )  THEN
	    it1 = 0
	  ELSE IF  ( ( pname1 .eq. 'TEMP' ) .or. ( pname1 .eq. 'TMPC' )
     +		     .or. ( pname1 .eq. 'DWPT' ) 
     +		     .or. ( pname1 .eq. 'DWPC' ) )  THEN
	    it1 = 1
	  ELSE IF ( ( pname1 .eq. 'TMPF' ) .or. ( pname1 .eq. 'DWPF' ) )
     +						    THEN
	    it1 = 2
	  ELSE IF ( ( pname1 .eq. 'TMPK' ) .or. ( pname1 .eq. 'DWPK' ) )
     +						    THEN
	    it1 = 3
	  ELSE IF ( ( pname1 .eq. 'THTA' ) .or. ( pname1 .eq. 'THTE' ) )
     +						    THEN
	    it1 = 4
	  ELSE 
	    it1 = 0
	END IF
C*
	IF  ( pname2 .eq. ' ' )  THEN
	    it2 = 0
	  ELSE IF  ( ( pname2 .eq. 'TEMP' ) .or. ( pname2 .eq. 'TMPC' )
     +		     .or. ( pname2 .eq. 'DWPT' ) .or. 
     +		     ( pname2 .eq. 'DWPC' ) )  THEN
	    it2 = 1
	  ELSE IF ( ( pname2 .eq. 'TMPF' ) .or. ( pname2 .eq. 'DWPF' ) )
     +						    THEN
	    it2 = 2
	  ELSE IF ( ( pname2 .eq. 'TMPK' ) .or. ( pname2 .eq. 'DWPK' ) )
     +						    THEN
	    it2 = 3
	  ELSE IF ( ( pname2 .eq. 'THTA' ) .or. ( pname2 .eq. 'THTE' ) )
     +						    THEN
	    it2 = 4
	  ELSE 
	    it2 = 0
	END IF
C
C*	Check for default parameters.
C
	IF  ( ( it1 .eq. 0 ) .and. ( it2 .ne. 0 ) )  it1 = it2
	IF  ( ( it2 .eq. 0 ) .and. ( it1 .ne. 0 ) )  it2 = it1
C
C*	Check that the types are the same.
C
	IF  ( it1 .ne. it2 )  THEN
	    iret = -6
            CALL GEPLOT ( ier )
	    CALL ER_WMSG  ( 'SNPROF', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Interpret user input and setup the axis.
C
	ilfdef = 1
	igfdef = 0
	itfdef = 1
	CALL IN_AXIS ( xaxis, ivc, skew, pname1, xmin, xmax,
     +                 ilfdef, igfdef, itfdef, xstrt, xstop, xlbl, 
     +                 nxlbl, ixlbfr, ixglfr, ixtmfr, iret )
C*
	IF ( iret .ne. 0 ) THEN
	  iret = -5
	  CALL ER_WMSG ( 'SNPROF', iret, ' ', ier )
	END IF
	RETURN
	END
