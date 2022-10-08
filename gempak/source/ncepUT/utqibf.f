	SUBROUTINE UT_QIBF  ( rval, cvalqd, rvalqa, rvalqr,
     +			      r8val, r8valqd, r8valqa, r8valqr, iret )
C************************************************************************
C* UT_QIBF								*
C*									*
C* Given an interface value and its associated MADIS QC values, this	*
C* subroutine returns all of them as REAL*8 BUFR values.  If the input	*
C* interface value is RMISSD, then the BUFR "missing" value is returned	*
C* for all output values.						*
C*									*
C* UT_QIBF  ( RVAL, CVALQD, RVALQA, RVALQR,				*
C*	      R8VAL, R8VALQD, R8VALQA, R8VALQR, IRET )			*
C*									*
C* Input parameters:							*
C*	RVAL		REAL		Interface value			*
C*	CVALQD		CHARACTER	QCD value associated with RVAL	*
C*	RVALQA		REAL		QCA value associated with RVAL	*
C*	RVALQR		REAL		QCA value associated with RVAL	*
C*									*
C* Output parameters:							*
C*	R8VAL		REAL*8		BUFR value			*
C*	R8VALQD		REAL*8		QCD value associated with R8VAL	*
C*	R8VALQA		REAL*8		QCA value associated with R8VAL	*
C*	R8VALQR		REAL*8		QCR value associated with R8VAL	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = all output values set to	*
C*					      BUFR "missing" because	*
C*					      input RVAL = RMISSD	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		06/01						*
C* J. Ator/NCEP		09/01	Modify to use FSL MADIS software	*
C* J. Ator/NCEP		01/02	Multiply QCA, QCR by 2 for BUFR storage	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BUFR.CMN'
C*
	CHARACTER*(*)	cvalqd
C*
	REAL*8		r8val, r8valqd, r8valqa, r8valqr
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	IF  ( .not. ERMISS ( rval ) )  THEN
	    iret = 0
	    r8val = rval
	    CALL UT_C2R8  ( cvalqd, 1, r8valqd, nr8valqd, ier2r8 )
	    IF  ( ier2r8 .ne. 0 )  THEN
		r8valqd = R8BFMS
	    END IF
	    r8valqa = rvalqa * 2
	    r8valqr = rvalqr * 2
	ELSE
	    iret = -1
	    r8val = R8BFMS
	    r8valqd = R8BFMS
	    r8valqa = R8BFMS
	    r8valqr = R8BFMS
	END IF
C*
	RETURN
	END
