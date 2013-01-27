	SUBROUTINE SNDINT ( nparms, levout, rdata,
     +			    nlevel, hghts, hdata, iret )
C************************************************************************
C* SNDINT								*
C*									*
C* This routine will interpolate data on pressure surfaces to data	*
C* on height surfaces.							*
C*									*
C* SNDINT ( NPARMS, LEVOUT, RDATA, NLEVEL, HGHTS, HDATA, IRET )		*
C*									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	LEVOUT		INTERGER	Number of output levels		*
C*	RDATA (LLMXLV)	REAL		Original sounding data		*
C*	NLEVEL		INTEGER		# of original sounding levels	*
C*	HGHTS (LEVOUT)	REAL		New height levels		*
C*									*
C* Output parameters:							*
C*	HDATA (LLMXLV)	REAL		Interpolated sounding data	*
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
	REAL		rdata(*), hghts(*), hdata(*)
C*
	REAL		adata(2), bdata(2), outdat(2)
	LOGICAL		intflg(2), angflg(2)
C------------------------------------------------------------------------
	intflg(1) = .false.
	intflg(2) = .true.
	angflg(1) = .false.
	angflg(2) = .false.
C
	DO  i = 1, nparms
	    hdata(0*nparms+i) = rdata(0*nparms+i)
	END DO
C
	DO  n = 2, nparms
	    DO  l = 2, nlevel
		h1 = hghts(l)
		outdat(2) = RMISSD
		iret = -24
		k = 1
		DO WHILE ( iret .ne. 0 .and. k .lt. levout )
		    adata(1) = rdata((k-1)*nparms+IHGHT)
		    bdata(1) = rdata( k   *nparms+IHGHT)
		    adata(2) = rdata((k-1)*nparms+n)
		    bdata(2) = rdata( k   *nparms+n)
		    CALL PC_INTH  ( h1, adata, bdata, 2, intflg,
     +				    angflg, 1, outdat, iret )
		    k = k + 1
		    hdata((l-1)*nparms+IHGHT) = h1
		    hdata((l-1)*nparms+n) = outdat(2)
		END DO
	    END DO
	END DO
	iret = 0
C*
	RETURN
	END
