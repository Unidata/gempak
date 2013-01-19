	SUBROUTINE SNSTAN ( ilev, thinc, x, pontha, nxtra, sxtra, 
     +			    pil, pilm1, tanmax, iret )
C************************************************************************
C* SNSTAN								*
C*									*
C* This subroutine checks adjoining isentropes to see if they are	*
C* tangled.  If so, the tangled points and new values are returned.	*
C*									*
C* SNSTAN ( ILEV, THINC, X, PONTHA, NXTRA, SXTRA, PIL, PILM1, TANMAX,	*
C*          IRET )							*
C*									*
C* Input parameters:							*
C*	ILEV		INTEGER		Level to check with level below	*
C*	THINC		REAL		Theta increment			*
C*	X (LLMAXD)	REAL		Coordinates at grid points	*
C*	PONTHA(LLMAXD,*)REAL		Pressure on isentropic surfaces	*
C*									*
C* Output parameters:							*
C*	NXTRA		INTEGER		Number of tangles found		*
C*	SXTRA (NXTRA)	REAL		X location of tangles		*
C*	PIL   (NXTRA)	REAL		New pressures on upper isentrope*
C*	PILM1 (NXTRA)	REAL		New pressures on lower isentrope*
C*	TANMAX		REAL		Maximum pressure diff at tangles*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		pontha (LLMAXD,*), sxtra (*), pil (*), 
     +			pilm1 (*), x (*)
	LOGICAL		cross
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	nxtra  = 0
	IF  ( ilev .le. 1 )  RETURN
	ilevm1 = ilev - 1
	tanmax = 0.0
C
C*	Loop through the points looking for tangled curves.
C
	cross = .false.
	pmax  = 0.0
	DO  i = 1, LLMAXD
	    ptop = pontha ( i, ilev )
	    pbot = pontha ( i, ilevm1 )
C
C*	    Check for tangled curves.  If found, save grid point where
C*	    tangle is deepest.
C
	    IF  ( ( .not. ERMISS (ptop )) .and. 
     +            ( .not. ERMISS (pbot )) .and.
     +		  ( ptop .gt. pbot ) ) THEN
		pdif = ptop - pbot
		IF  ( pdif .gt. pmax ) THEN
		    pmax = pdif
		    igrd = i
		END IF
		cross = .true.
C
C*		Points are not tangled.  Check to see if this terminates
C*		a tangled area.  If so, save values.
C
	      ELSE IF ( cross ) THEN
		nxtra = nxtra + 1
		ptop = pontha (igrd, ilev)
		pbot = pontha (igrd, ilevm1)
		pav  = ( ptop + pbot ) / 2.
		pil (nxtra) = pav - thinc
		pilm1 (nxtra) = pav + thinc
		sxtra (nxtra) = x ( igrd )
		cross = .false.
		IF ( pmax .gt. tanmax ) tanmax = pmax
		pmax  = 0.0
	    END IF
	END DO
C*
	RETURN
	END
