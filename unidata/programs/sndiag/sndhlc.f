	SUBROUTINE SNDHLC ( nparms, nlevel, hdata, iret )
C************************************************************************
C* SNDHLC								*
C*									*
C* This routine will compute the Helicity for a sounding processed	*
C* with SNDIAG.								*
C*									*
C* SNDHLC ( NPARMS, NLEVEL, HDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
C*									*
C* Input/Output parameters:						*
C*	HDATA (LLMXLV)	REAL		Interpolated sounding data	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	REAL		hdata(*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	DO  i = 1, nlevel
	    IF  ( ( .not. ERMISS(hdata((i-1)*nparms+IUWND)) ) .and.
     +            ( .not. ERMISS(hdata((i-1)*nparms+IVWND)) ) .and.
     +            ( .not. ERMISS(hdata(0*nparms+IUWND)) ) .and.
     +            ( .not. ERMISS(hdata(0*nparms+IVWND)) ) )  THEN
		IF  ( hdata((i-1)*nparms+IHGHT) .ne.
     +		      hdata(0*nparms+IHGHT) )  THEN
		    term1 = hdata((i-1)*nparms+IVWND) * 
     +			(hdata((i-1)*nparms+IUWND)-
     +				hdata(0*nparms+IUWND)) /
     +			(hdata((i-1)*nparms+IHGHT)-
     +				hdata(0*nparms+IHGHT))
		    term2 = hdata((i-1)*nparms+IUWND) * 
     +		    	(hdata((i-1)*nparms+IVWND)-
     +				hdata(0*nparms+IVWND)) /
     +		    	(hdata((i-1)*nparms+IHGHT)-
     +				hdata(0*nparms+IHGHT))
		    hdata((i-1)*nparms+IHELC) = term1 - term2
		ELSE
		    hdata((i-1)*nparms+IHELC) = RMISSD
		END IF
	    END IF
	END DO
C*
	RETURN
	END
