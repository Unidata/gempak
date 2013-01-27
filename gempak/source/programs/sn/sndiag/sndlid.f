	SUBROUTINE SNDLID ( nlun, lun, dattim, nparms, levout, rdata,
     +			    iret )
C************************************************************************
C* SNDLID								*
C*									*
C* This program will compute the lid strength index and related 	*
C* variables.								*
C*									*
C* SNDLID ( NLUN, LUN, DATTIM, NPARMS, LEVOUT, RDATA, IRET )		*
C*									*
C* Input parameters:							*
C*	NLUN		INTEGER		Number of file numbers		*
C*	LUN (NLUN)	INTEGER		File numbers			*
C*	DATTIM		CHAR*		Date/time			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	LEVOUT		INTEGER		Number of levels		*
C*	RDATA (LLMXLV)	REAL		Sounding data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Inspired by PSU code LSI.FOR	*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER*(*)	dattim
	INTEGER		lun(*)
	REAL		rdata(*)
C*
	INTEGER		pres(LLMXLV), temp(LLMXLV), dwpt(LLMXLV),
     +			rrhbrk, bseinv
C------------------------------------------------------------------------
	iret = 0
C
C*	Write the output header.
C
	DO  j = 1, nlun
	    WRITE ( lun(j), 1000 )
	END DO
1000	FORMAT ( /,' From the PENN STATE Lid Strength Index Code:',/ )
C
C*	Set the arrays for the TEMP, DWPT, and PRES.
C
	DO i = 1, levout
	    temp(i) = INT ( PR_TMCK ( rdata((i-1)*nparms+ITEMP) ) )
	    dwpt(i) = INT ( PR_TMCK ( rdata((i-1)*nparms+IDWPT) ) )
	    pres(i) = INT (           rdata((i-1)*nparms+IPRES)   )
	END DO
C
C*	Find pressure levels of RH discontinuities and lid bases.
C
	CALL SNDRHB ( nlun, lun, levout, pres, temp, dwpt,
     +		      iemlflg, rrhbrk, bseinv, ier )
C*
	RETURN
	END
