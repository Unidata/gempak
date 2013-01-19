	SUBROUTINE PC_PHGT  ( hght, level1, level2, pres, iret )
C************************************************************************
C* PC_PHGT								*
C*									*
C* This subroutine computes the value of the pressure in a dataset	*
C* for a given value of height.  The computation uses the vertical	*
C* data stored internally by the PC package.				*
C*									*
C* PC_PHGT  ( HGHT, LEVEL1, LEVEL2, PRES, IRET )			*
C*									*
C* Input parameters:							*
C*	HGHT		REAL		Height				*
C*	LEVEL1		INTEGER		Level at or below height	*
C*	LEVEL2		INTEGER	  	Level above height		*
C*									*
C* Output parameters:							*
C*	PRES		REAL		Pressure			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-25 = incorrect levels		*
C*					-26 = vdata not computable	*
C*					-27 = max iterations done	*
C*					-28 = bad or missing data	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* T. Lee/GSC		 8/97	Moved p1 outside EXP in PGUESS function	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE    	'pccmn.cmn'
C*
	LOGICAL    	BETWEN
C
C*      Following include is for the missing data statement function.
C*      It must be before the other statement function declarations,
C*      since it is used in these other functions.
C
        INCLUDE    'ERMISS.FNC'
C
C*	Statement function to compute pressure, the logarithm of which 
C*	is assumed to vary linearly with height.
C
	PGUESS () = p1 * EXP ( (hght-h1) * ALOG (p2/p1) / (h2-h1) )
C
C*	Statement function to check if x < a < y.
C
	BETWEN (x, y, a) = ( (.not. ERMISS (x)) .and.
     +                       (.not. ERMISS (y)) .and.
     +			      (x .le. a) .and. (a .le. y) )
C------------------------------------------------------------------------
C*	Check that data will be in vertical table.
C
	iret = 0
	pres = RMISSD
	IF  ( ( .not. vcomp (1) ) .or. ( .not. vcomp (3) ) )  THEN
	    iret = -26
	    RETURN
	END IF
C
C*	Find the bounding values of pressure and height.  Use index of
C*	1 for lower value of height and index of 2 for higher value.
C
	IF  ( vdata ( 3, level1 ) .lt. vdata ( 3, level2 ) )  THEN
	    p1 = vdata ( 1, level1 )
	    p2 = vdata ( 1, level2 )
	    h1 = vdata ( 3, level1 )
	    h2 = vdata ( 3, level2 )
	  ELSE
	    p1 = vdata ( 1, level2 )
	    p2 = vdata ( 1, level1 )
	    h1 = vdata ( 3, level2 )
	    h2 = vdata ( 3, level1 )
	END IF
C
C*	Check that this required data is not missing.
C
	IF  ( ERMISS ( p1 ) .or. ERMISS ( p2 ) .or. 
     +	      ERMISS ( h1 ) .or. ERMISS ( h2 ) )  THEN
	    iret = -28
	  ELSE IF  ( ( p1 .eq. 0. ) .or. ( h1 .eq. h2 ) .or.
     +		     ( .not. BETWEN ( h1, h2, hght ) ) )  THEN
	    iret = -28
	END IF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Use function PGUESS toc compute pressure.
C
	pres  = PGUESS ()
C*                                        
	RETURN
	END
