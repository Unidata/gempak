	FUNCTION SND_SHD  ( datain, nparm, k, deltaz, ivcord )
C************************************************************************
C* SND_SHD								*
C*									*
C* This function computes the wind shear direction for a layer.		*
C*									*
C* SND_SHD  ( DATAIN, NPARM, K, DELTAZ, IVCORD )			*
C*									*
C* Input parameters:							*
C*	DATAIN (LLMXLV)	REAL		Interpolated sounding data	*
C*	NPARM		INTEGER		Number of parameters		*
C*	K		INTEGER		Current sounding level		*
C*	DELTAZ		REAL		Height interval			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*									*
C* Output parameters:							*
C*	SND_SHD		REAL		Wind shear direction		*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 5/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	REAL            datain (*)
C*
	REAL    	stndb (10), stndt (10), stndl (10)
	CHARACTER       cvalue*20
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	SND_SHD = RMISSD
C
C*	Get information over a depth.
C
c	dfdpth = 0.
c	idfcrd = 3
c	CALL PC_DPTH  ( datain, nparm, clev, ivcord, dfdpth, idfcrd, 1,
c     +			depth, idcord, stndl, stndb, stndt, ier )
C
C*	Check for missing data.
C
c	IF  ( ier .ne. 0 )  RETURN
c	DO  i = 4, 6
c	    IF ( ERMISS ( stndb (i) ) ) RETURN
c	    IF ( ERMISS ( stndt (i) ) ) RETURN
c	END DO
c	depth = stndt (6) - stndb (6)
c	IF  ( depth .eq. 0.0 )  RETURN
C
C*	Do shear calculation.
C
c	CALL PC_GCND ( '$', 1, dir, cvalue, ier )
c	utop = stndt (4)
c	vtop = stndt (5)
c	ubot = stndb (4)
c	vbot = stndb (5)
	depth = deltaz
	utop = datain((k-1+1)*nparm+IUABS)
	vtop = datain((k-1+1)*nparm+IVABS)
	ubot = datain((k-1-1)*nparm+IUABS)
	vbot = datain((k-1-1)*nparm+IVABS)
c	IF ( ier .ne. 0 ) THEN
	    dudz = ( utop - ubot ) / ( 2. * depth )
	    dvdz = ( vtop - vbot ) / ( 2. * depth )
	    SND_SHD = PR_DRCT ( dudz, dvdz )
c	  ELSE
c	    SND_SHD = dir
c	END IF
C*
	RETURN
	END
