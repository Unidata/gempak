	SUBROUTINE SNDWND ( storm, crdrot, nparms, nlevel, hdata, iret )
C************************************************************************
C* SNDWND								*
C*									*
C* This routine will calculate the winds relative to the storm/wave	*
C* motion and coordinate rotation.					*
C*									*
C* SNDWND ( STORM, CRDROT, NPARMS, NLEVEL, HDATA, IRET )		*
C*									*
C* Input parameters:							*
C*	STROM		CHAR*		Storm speed and direction	*
C*	CRDROT		CHAR*		Coordinate rotation		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVLE		INTEGER		Number of levels		*
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
	CHARACTER*(*)	storm, crdrot
	REAL		hdata(*)
C*
	REAL		rarr(2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_RLST ( storm, '/', 0., 2, rarr, isnum, ier )
	stsped = rarr(1)
	stdrct = rarr(2)
	ust = stsped * ( -SIN(stdrct*DTR) )
	vst = stsped * ( -COS(stdrct*DTR) )
C
	CALL ST_CRNM ( crdrot, rotang, ier )
	IF  ( ier .ne. 0 )  rotang = 0.
	rotang = 270. - rotang
C
	DO  i = 1, nlevel
	    IF  ( ( .not. ERMISS(hdata((i-1)*nparms+IUWND)) ) .and.
     +		  ( .not. ERMISS(hdata((i-1)*nparms+IVWND)) ) )  THEN
		ur = hdata((i-1)*nparms+IUWND) - ust
		vr = hdata((i-1)*nparms+IVWND) - vst
		hdata((i-1)*nparms+IUWND) = ur * COS(rotang*DTR) +
     +					    vr * SIN(rotang*DTR)
		hdata((i-1)*nparms+IVWND) = vr * COS(rotang*DTR) -
     +					    ur * SIN(rotang*DTR)
	    END IF
	END DO
C*
	RETURN
	END
