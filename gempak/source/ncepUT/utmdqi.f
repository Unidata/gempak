	SUBROUTINE UT_MDQI  ( idxstn, idxvar,
     +			      obs, qcd, iqca, iqcr, mxstns,
     +			      rval, cvalqd, rvalqa, rvalqr, iret )
C************************************************************************
C* UT_MDQI								*
C*									*
C* This subroutine retrieves an interface value (and all associated QC	*
C* values!) for a specified variable at a specified station from the	*
C* MADIS software observation and QC arrays.				*
C*									*
C* UT_MDQI  ( IDXSTN, IDXVAR,						*
C*	      OBS, QCD, IQCA, IQCR, MXSTNS,				*
C*	      RVAL, CVALQD, RVALQA, RVALQR, IRET )			*
C*									*
C* Input parameters:							*
C*	IDXSTN		INTEGER		Station index into MADIS arrays	*
C*	IDXVAR		INTEGER		Variable index into MADIS arrays*
C*	OBS(*,*)	REAL		MADIS observations array	*
C*	QCD(*,*)	CHARACTER	QCD values associated with OBS	*
C*	IQCA(*,*)	INTEGER		QCA values associated with OBS	*
C*	IQCR(*,*)	INTEGER		QCR values associated with OBS	*
C*	MXSTNS		INTEGER		Maximum number of stations	*
C*					within MADIS arrays		*
C*									*
C* Output parameters:							*
C*	RVAL		REAL		Interface value			*
C*	CVALQD		CHARACTER	QCD value associated with RVAL	*
C*	RVALQA		REAL		QCA value associated with RVAL	*
C*	RVALQR		REAL		QCA value associated with RVAL	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		06/01						*
C* J. Ator/NCEP		09/01	Modify to use FSL MADIS software	*
C************************************************************************
	REAL		obs ( mxstns, * )
C*
	CHARACTER	qcd ( mxstns, * ), cvalqd
C*
	INTEGER		iqca ( mxstns, * ), iqcr ( mxstns, * )
C-----------------------------------------------------------------------
	iret = 0
C
	rval = UT_MDRI ( obs ( idxstn, idxvar ) )
C
	cvalqd = qcd ( idxstn, idxvar )
	rvalqa = UT_MDRI ( FLOAT ( iqca ( idxstn, idxvar ) ) )
	rvalqr = UT_MDRI ( FLOAT ( iqcr ( idxstn, idxvar ) ) )
C*
	RETURN
	END
