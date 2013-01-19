	SUBROUTINE PC_FNDL  ( data, nprm, nlev, icol, isf, vlev,
     +			      botup, eps, ivert, level1, level2, itype,
     +			      iret )
C************************************************************************
C* PC_FNDL								*
C*									*
C* This subroutine searches the station data for a specific vertical	*
C* level.								*
C*									*
C* PC_FNDL ( DATA, NPRM, NLEV, ICOL, ISF, VLEV, BOTUP, EPS, IVERT,	*
C*	     LEVEL1, LEVEL2, ITYPE, IRET )				*
C*									*
C* Input parameters:							*
C*	DATA		REAL		Station data			*
C*	 (NPRM,NLEV)							*
C*	NPRM		INTEGER		Number of station parameters	*
C*	NLEV		INTEGER		Number of levels		*
C*	ICOL		INTEGER		Column with vertical cord	*
C*	ISF		INTEGER		Surface flag			*
C*	VLEV		REAL		Vertical level to find		*
C*	BOTUP		LOGICAL		Bottom to top isentropic srch	*
C*	EPS		REAL		Value to test for equality	*
C*	IVERT		INTEGER		Vertical coordinate		*
C*									*
C* Output parameters:							*
C*	LEVEL1		INTEGER		Level below or at VLEV		*
C*	LEVEL2		INTEGER		Level above VLEV		*
C*	ITYPE		INTEGER		Type of level			*
C*					  1 = data at level1		*
C*					  2 = data between levels	*
C*					  3 = data below lowest level	*
C*					  4 = data above top level	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = no surface data		*
C*					-11 = no valid data		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	10/89	Corrected problem for 2 level data	*
C* M. desJardins/GSFC	 2/91	Fix search for levels below surface	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		data ( NPRM, NLEV )
	LOGICAL		botup
C*
	LOGICAL		found, done, start, getbig
C
C*	Statement functions.
C
	LOGICAL		equal, betwen
C*
        INCLUDE 'ERMISS.FNC'
C*
	EQUAL ( x ) = ( ( .not. ERMISS (x) ) .and.
     +			( ABS ( x - vlev ) .lt. eps ) )
C*
	BETWEN ( x, y ) = ( ( .not. ERMISS (x) ) .and.
     +			    ( .not. ERMISS (y) ) .and.
     +			    ( ( ( x .lt. y ) .and. ( x .lt. vlev ) .and.
     +				( vlev .lt. y ) .and. getbig ) .or.
     +			      ( ( y .lt. x ) .and. ( y .lt. vlev ) .and.
     +				( vlev .lt. x ) .and.
     +				( .not. getbig ) ) ) )
C-----------------------------------------------------------------------
C*	Initialize.
C
	iret   = 0
	itype  = -1
	level1 = 0
	level2 = 0
	IF  ( ivert .eq. 1 )  THEN
	    getbig = .false.
	  ELSE
	    getbig = .true.
	END IF
C
C*	Check for surface data if vlev = 0.0 .
C
	IF  ( vlev .eq. 0.0 )  THEN
	    IF  ( isf .eq. 0 )  THEN
		level1 = 1
		itype = 1
	      ELSE
		iret = -9
	    ENDIF
	    RETURN
	END IF
C
C*	Check for top level (vlev = -1.0).
C
	IF  ( vlev .eq. -1.0 )  THEN
	    level1 = nlev
	    itype = 1
	    RETURN
	END IF
C
C*	Find first and last levels to search depending on botup flag.
C
	IF  ( botup )  THEN
	    levbot = 1
	    levtop = nlev
	    inc    = 1
	  ELSE
	    levbot = nlev
	    levtop = 1
	    inc    = -1
	END IF
C
C*	Search for the requested data
C
	start  = .false.
	found  = .false.
	done   = .false.
	i      = levbot
	olddat = RMISSD
C*
	DO WHILE  ( ( .not. done ) .and. ( .not. found ) )
	    curdat = data ( icol, i )
	    IF  ( EQUAL ( curdat ) )  THEN
		level1 = i
		itype  = 1
		found  = .true.
	      ELSE IF  ( .not. ERMISS ( curdat ) )  THEN
		datlas = curdat
		IF  ( .not. start )  THEN
		    start  = .true.
		    levstr = i
		    datstr = curdat
		    olddat = curdat
		    levold = i
		    IF  ( i .eq. levtop )  done = .true.
		    i      = i + inc
		  ELSE IF  ( BETWEN ( olddat, curdat ) )  THEN
		    level1 = levold
		    level2 = i
		    itype  = 2
		    found  = .true.
		  ELSE IF  ( i .eq. levtop )  THEN
		    done   = .true.
		  ELSE
		    IF  ( ( getbig .and. ( curdat .gt. olddat ) )
     +					.or.
     +			  ( .not. getbig .and.
     +				( curdat .lt. olddat ) ) )  THEN
			olddat = curdat
			levold = i
		    END IF
		    i = i + inc
		ENDIF
	      ELSE IF  ( i .eq. levtop )  THEN
		done = .true.
	      ELSE
		i    = i + inc
	    END IF
	END DO
C
C*	If level has not been found, check to see if it is below or
C*	above the sounding.
C
	IF  ( .not. found )  THEN
	    IF  ( .not. start )  THEN
		iret = -11
	      ELSE IF  ( datstr .eq. datlas )  THEN
		itype = 4
	      ELSE IF  ( datstr .lt. datlas )  THEN
		IF  ( vlev .lt. datstr )  THEN
		    itype = 4
		    IF  ( botup )  itype = 3
		  ELSE
		    itype = 3
		    IF  ( botup )  itype = 4
		END IF
	      ELSE
		IF  ( vlev .lt. datstr )  THEN
		    itype = 3
		    IF  ( botup )  itype = 4
		  ELSE
		    itype = 4
		    IF  ( botup )  itype = 3
		END IF
	    END IF
	END IF
C*
	RETURN
	END
