	FUNCTION SND_BVS  ( datain, nparm, k, deltaz, ivcord )
C************************************************************************
C* SND_BVS								*
C*									*
C* This function computes the square of the Brunt-Vaisala Frequency.	*
C*									*
C* SND_BVS  ( DATAIN, NPARM, K, DELTAZ, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN (LLMXLV)	REAL		Interpolated sounding data	*
C*	NPARM		INTEGER		Number of parameters		*
C*	K		INTEGER		Current level			*
C*	DELTAZ		INTEGER		Level increment			*
C*	IVCORD		INTEGER		Vertical coordinate type	*
C*									*
C* Output parameters:							*
C*	SND_BVS		REAL		Brunt-Vaisala frequency		*
C*									*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 8/90						*
C* S. Jacobs/SSAI	 4/92	Changed initial depth to user input	*
C* J. Whistler/SSAI	11/92	Cleaned up and added check for missing	*
C*				data					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	REAL		datain (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	SND_BVS = RMISSD
C
C*	Get data at this layer, unless requested otherwise.
C
	IF ( ERMISS ( datain((k-1+1)*nparm+ITHTV) ) .or.
     +	     ERMISS ( datain((k-1+1)*nparm+ITHTV) ) )  THEN 
	    RETURN
	ELSE
	    depth = deltaz * 2.
	    thtop = datain((k-1+1)*nparm+ITHTV)
	    thbot = datain((k-1-1)*nparm+ITHTV)
            theav = ( thtop + thbot ) / 2.
C
C*	    Compute N**2.
C
	    IF ( depth .gt. 0. )  THEN
	        bvfsqd = ( GRAVTY / theav ) * ( thtop - thbot ) / depth
	    ELSE 
		bvfsqd = 0.0
	    END IF
C
	    IF  ( bvfsqd .lt. 0.0 )  RETURN
	    SND_BVS = bvfsqd
	END IF
C*
	RETURN
	END
