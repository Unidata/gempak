	SUBROUTINE SNSPTS ( ilev, nstn, pdat, sloc, ns, ss, pp, iret )
C************************************************************************
C* SNSPTS								*
C*									*
C* This subroutine finds the pressures on an isentrope at the stations	*
C* for the cross sections program.					*
C*									*
C* SNSPTS ( ILEV, NSTN, PDAT, SLOC, NS, SS, PP, IRET )			*
C*									*
C* Input parameters:							*
C*	ILEV		INTEGER		Level number			*
C*	NSTN		INTEGER		Number of stations		*
C*	PDAT (LLTMCX,*)	REAL		Pressure at stations		*
C*	SLOC (NSTN)	REAL		Location of stations		*
C*									*
C* Output parameters:							*
C*	NS		INTEGER		Number of stations reporting	*
C*	SS (NS)		REAL		Location of reporting stations	*
C*	PP (NS)		REAL		Pressure at reporting stations	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 3/91	Added LLTMCX				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		ss (*), pp (*), pdat (LLTMCX,*), sloc (*)
C*
	INCLUDE 	'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	ns = 0
C
C*	Loop though all the stations.  If data is not missing, add it
C*	to the output arrays.
C
	DO  is = 1, nstn
	    IF  ( .not. ERMISS ( pdat (is, ilev) ) ) THEN
		ns = ns + 1
		pp (ns) = pdat (is, ilev)
		ss (ns) = sloc (is)
	    END IF
	END DO
C*
	RETURN
	END
