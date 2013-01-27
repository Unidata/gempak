	SUBROUTINE SNDSHR ( nparms, nlevel, delz, hdata, iret )
C************************************************************************
C* SNDSHR								*
C*									*
C* This routine will compute the shear components of the wind.		*
C*									*
C* SNDSHR ( NPARMS, NLEVEL, DELZ, HDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
C*	DELZ		CHAR*		Height intervals		*
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
	CHARACTER*(*)	delz
	REAL		hdata(*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	CALL ST_CRNM ( delz, deltaz, ier )
	IF  ( ier .ne. 0 ) deltaz = 500.
C
	DO  i = 2, nlevel-1
	    IF  ( ERMISS(hdata((i-1+1)*nparms+IUWND)) .or. 
     +		  ERMISS(hdata((i-1-1)*nparms+IUWND)) )  THEN
		hdata((i-1)*nparms+IDUDZ) = RMISSD
	    ELSE
	    	hdata((i-1)*nparms+IDUDZ) =
     +			(hdata((i-1+1)*nparms+IUWND)-
     +			 hdata((i-1-1)*nparms+IUWND)) / (2.*deltaz)
	    END IF
	    IF  ( ERMISS(hdata((i-1+1)*nparms+IVWND)) .or. 
     +		  ERMISS(hdata((i-1-1)*nparms+IVWND)) )  THEN
		hdata((i-1)*nparms+IDUDZ) = RMISSD
	    ELSE
		hdata((i-1)*nparms+IDVDZ) =
     +			(hdata((i-1+1)*nparms+IVWND)-
     +			 hdata((i-1-1)*nparms+IVWND)) / (2.*deltaz)
	    END IF
	END DO
C*
	RETURN
	END
