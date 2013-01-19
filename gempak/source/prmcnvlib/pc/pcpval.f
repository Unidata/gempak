	SUBROUTINE PC_PVAL  ( thta, level1, level2, pres, iret )
C************************************************************************
C* PC_PVAL								*
C*									*
C* This subroutine computes the value of the pressure in a dataset	*
C* for a given value of theta.  The computation uses the vertical	*
C* vertical data stored internally by the PC package.			*
C*									*
C* PC_PVAL  ( THTA, LEVEL1, LEVEL2, PRES, IRET )			*
C*									*
C* Input parameters:							*
C*	THTA		REAL		Value of theta			*
C*	LEVEL1		INTEGER		Level at or below THTA		*
C*	LEVEL2		INTEGER	  	Level above THTA		*
C*									*
C* Output parameters:							*
C*	PRES		REAL		Pressure			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-23 = bad or missing data	*
C*					-25 = incorrect levels		*
C*					-26 = isentropic data not comp	*
C*					-27 = max iterations done	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 6/87	Exit loop when guess reaches real	*
C*				precision				*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE    	'pccmn.cmn'
C*
	REAL       	adata (4), bdata (4), cdata (4)
	LOGICAL    	done, BETWEN
	LOGICAL    	intflg (2), angflg (2)
	PARAMETER  	(EPS = .001)
C
C*      This include for statement function for missing data.
C       Put before other statement functions.
C
        INCLUDE    'ERMISS.FNC'
C

C*
	DATA       	intflg /2*.true./, angflg /2*.false./
	DATA	   	iter /100/
C
C*	Statement function to guess pressure.  It is assumed that theta
C*	varies linearly with log pressure.
C
	PGUESS() = EXP ( ALOG (p1) + ( thta - th1 ) * 
     +			 ALOG ( p2 / p1 ) / ( th2 - th1 ) )
C
C*	Statement function to check if x < a < y.
C
	BETWEN (x, y, a) = ( ( .not. ERMISS (x) )
     +                          .and. ( .not. ERMISS (y)) 
     +				.and. (x .le. a) .and. (a .le. y) )
C------------------------------------------------------------------------
C*	Check that data will be in vertical table.
C
	iret = 0
	pres = RMISSD
	IF  ( ( .not. vcomp (1) ) .or. ( .not. vcomp (2) ) .or. 
     +	      ( .not. vcomp (5) ) )  THEN
	    iret = -26
	    RETURN
	END IF
C
C*	Find the bounding values of pressure and theta.  Use index of
C*	1 for lower value of theta and index of 2 for higher value.
C
	IF  ( vdata ( 2, level1 ) .lt. vdata ( 2, level2 ) )  THEN
	    p1  = vdata (1, level1)
	    p2  = vdata (1, level2)
	    th1 = vdata (2, level1)
	    th2 = vdata (2, level2)
	    t1  = vdata (5, level1)
	    t2  = vdata (5, level2)
	  ELSE
	    p1  = vdata (1, level2)
	    p2  = vdata (1, level1)
	    th1 = vdata (2, level2)
	    th2 = vdata (2, level1)
	    t1  = vdata (5, level2)
	    t2  = vdata (5, level1)
	END IF
C
C*	Check that this required data is not missing
C
	IF  ( ERMISS ( p1 )  .or. ERMISS ( p2 )  .or. 
     +	      ERMISS ( th1 ) .or. ERMISS ( th2 ) .or. 
     +	      ERMISS ( t1 )  .or. ERMISS ( t2 ) )  THEN
	    iret = -23
	  ELSE IF  ( ( p1 .eq. 0. ) .or. ( th1 .eq. th2 ) .or.
     +		     ( .not. BETWEN ( th1, th2, thta ) ) )  THEN
	    iret = -23
	END IF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Make initial guess for pressure and fill adata, bdata with pressure
C*	and temperature for interpolation.
C
	pg    = PGUESS ()
	pgold = RMISSD
	adata (1) = p1
	bdata (1) = p2
	adata (2) = t1
	bdata (2) = t2
	done  = .false.
	kiter = 1
C*
	DO WHILE  ( .not. done )
	    cdata (2) = RMISSD
	    CALL PC_INTP  ( pg, adata, bdata, 2, intflg, angflg, cdata,
     +			    ier )
	    tg  = cdata (2)
	    thg = PR_THTA ( tg, pg )
	    IF  ( ABS ( thg - thta ) .lt. EPS )  THEN
		pres = pg
		done = .true.
	      ELSE IF  ( pg .eq. pgold )  THEN
		pres = pg
		done = .true.
	      ELSE IF  ( BETWEN ( thta, th2, thg ) )  THEN
		p2  = pg
		th2 = thg
	      ELSE IF  ( BETWEN ( th1, thta, thg ) )  THEN
		p1  = pg
		th1 = thg
	      ELSE
		done = .true.
		iret = -27
	    END IF
	    pgold = pg
	    pg    = PGUESS ()
	    kiter = kiter + 1
	    IF  ( kiter .gt. iter )  THEN
		done = .true.
		iret = -27
	    END IF
	END DO
C*
	RETURN
	END
