	SUBROUTINE SNSUNT ( ilev, maxl, pontha, x, ig1, ig2, iret )
C************************************************************************
C* SNSUNT								*
C*									*
C* This subroutine untangles isentropes for the cross section program.	*
C*									*
C* SNSUNT ( ILEV, MAXL, PONTHA, X, IG1, IG2, IRET )			*
C*									*
C* Input parameters:							*
C*	ILEV		INTEGER		Level number			*
C*	MAXL		INTEGER		Number of levels		*
C*	PONTHA 								*
C*	 (LLMAXD,MAXL)	REAL		Pressure on isentropes		*
C*	X (LLMAXD)	REAL		Location of grid points		*
C*	IG1		INTEGER		First grid point tangled	*
C*	IG2		INTEGER		Last  grid point tangled	*
C*									*
C* Output parameters:							*
C*	IRET			INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Woytek		 9/82	Originally called CSTCRS		*
C* M. desJardins/GSFC	 2/85						*
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		pontha (LLMAXD,*), x (*)
	LOGICAL		found
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	ilevm1 = ilev - 1
	ilevm2 = ilev - 2
C
C*	Try to find level above ilev which is not tangled with (ilev-1).
C
	ilevup = ilev + 1
	found  = .false.
	DO WHILE ( (.not. found) .and. (ilevup .le. maxl) )
	    found = .true.
C
C*	    Check each point on the upper curve for intersection with lower.
C
	    DO  ig = ig1-1, ig2+1
		pup   = pontha (ig, ilevup)
		pdown = pontha (ig, ilevm1)
		IF ( (ERMISS (pup)) .or. (ERMISS (pdown)) .or.
     +		     ( pup .ge. pdown) ) found = .false.
	    END DO
	    IF ( .not. found ) ilevup = ilevup + 1
	END DO
C
C*	Check that data exists on lev one point outside tangled range.
C
	IF ( (ERMISS (pontha (ilev,ig1-1))) .or.
     +	     (ERMISS (pontha (ilev,ig2+1)))) found = .false.
C*
	IF  ( .not. found ) THEN
C
C*	    If an upper bounding level was not found, trace previous layer.
C
	    DO  ig = ig1, ig2
		pm1 = pontha ( ig, ilevm1 )
		pm2 = pontha ( ig, ilevm2 )
		IF  ( ( .not. ERMISS (pm1)) .and. 
     +		      ( .not. ERMISS (pm2)))
     +		    pontha (ig,ilev) = 2 * pontha (ig,ilevm1) -
     +					pontha (ig, ilevm2)
	    END DO
C
C*	    Otherwise, use two bounding layers to redraw tangled line.
C
	  ELSE
C
C*	    Get the ratio of the pressure differences for the lower,
C*	    upper and current level outside the tangle.
C
	    dpul = pontha (ig1-1, ilevup) - pontha (ig1-1, ilev)
	    dpll = pontha (ig1-1, ilev)   - pontha (ig1-1, ilevm1)
	    dpur = pontha (ig2+1, ilevup) - pontha (ig2+1, ilev)
	    dplr = pontha (ig2+1, ilev)   - pontha (ig2+1, ilevm1)
	    ratio1 = dpul / dpll
	    ratio2 = dpur / dplr
	    dx     = x (ig2+1) - x (ig1-1)
C
C*	    Loop though all the tangled points.
C
	    DO  ig = ig1, ig2
C
C*		Get scaled ratio of pressure differences to use.
C
		delx = x (ig) - x (ig1-1)
		dist = delx / dx
		ratio = ( ratio2 - ratio1 ) * dist + ratio1
C
C*		Compute new pressure value between ptop and pbot.
C
		ptop  = pontha (ig, ilevup)
		pbot  = pontha (ig, ilevm1)
		pontha (ig, ilev) = (ptop + ratio*pbot) / (ratio+1.)
	    END DO
	END IF
C*
	RETURN
	END
