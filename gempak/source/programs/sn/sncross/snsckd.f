	SUBROUTINE SNSCKD ( maxl, pontha, x, iret )
C************************************************************************
C* SNSCKD								*
C*									*
C* This subroutine makes a final check of the isentropes for tangles.	*
C* Any points found tangled are fixed.					*
C*									*
C* Note that the corrected values are returned in the PONTHA array.	*
C*									*
C* SNSCKD ( MAXL, PONTHA, X, IRET )					*
C*									*
C* Input parameters:							*
C*	MAXL		INTEGER		Maximum number of levels	*
C*	PONTHA								*
C*	  (LLMAXD,MAXL) REAL		Pressure on isentropes		*
C*	X (LLMAXD)	REAL		Location of grid points		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		pontha (LLMAXD,*), x(*)
	LOGICAL		cross
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check every level starting at level 3 to see if it is tangled
C*	with next lower level.  IG1 and IG2 are the first and last
C*	consecutive grid points that are tangled.
C
	DO  ilev = 3, maxl
C*
	    ilevm1 = ilev - 1
	    cross  = .false.
	    DO  ig = 1, LLMAXD
		ptop = pontha ( ig, ilev )
		pbot = pontha ( ig, ilevm1 )
		IF ( ( .not. ERMISS ( ptop  )) .and. 
     +               ( .not. ERMISS ( pbot  )) .and.
     +		     ( ptop .ge. pbot ) ) THEN
		    IF  ( .not. cross ) THEN
			ig1 = ig
			ig2 = ig
			cross = .true.
		      ELSE
			ig2 = ig
		    END IF
C
C*		    If this point terminates a tangle, untangle.
C
		  ELSE IF ( cross ) THEN
		    cross = .false.
		    CALL SNSUNT ( ilev, maxl, pontha, x, ig1, ig2, ier )
		END IF
	    END DO
	END DO
C*
	RETURN           
	END
