	SUBROUTINE NAMLSD ( bufr, nlvl, np, sncfac, sndata, iret )
C************************************************************************
C* NAMLSD								*
C*									*
C* This subroutine loads data from the input array BUFR to the output	*
C* array SNDATA.  The conversion multiplier SNCFAC is applied to	*
C* the data.								*
C*									*
C* NAMLSD  ( BUFR, NLVL, NP, SNCFAC, SNDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	BUFR   (*)	REAL		Array of input values		*
C*	NLVL		INTEGER		Number of levels 		*
C*	NP		INTEGER		Number of parameters		*
C*	SNCFAC (NP)	REAL		Conversion factor array		*
C*									*
C* Output parameters:							*
C*	SNDATA (*)	REAL		Array of output values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* D. Kidwell/NCEP	12/98	SNMLSD -> NAMLSD			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		bufr (*), sndata (*), sncfac (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	indx = 0
	DO k = 1, nlvl
	    DO ip = 1, np
		indx = indx + 1
	        IF ( .not. ERMISS ( bufr (indx ) ) ) THEN
		    sndata ( indx ) = bufr ( indx ) * sncfac (ip)
		ELSE
		    sndata ( indx ) = RMISSD
		END IF
	    END DO
	END DO
C*
	RETURN
	END
