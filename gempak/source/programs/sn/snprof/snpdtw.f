	SUBROUTINE SNPDTW  ( ip1arr, ip2arr, iwncol, data, nlev, idtype,
     +                       ystrt, ystop, temp, ntemp, dwpt, ndwpt,
     +                       wind, nwind, xmin, xmax, iret )
C************************************************************************
C* SNPDTW								*
C*									*
C* This subroutine gets profile and wind data for SNPROF.		*
C*									*
C* NOTE:  RDATA, TEMP, DWPT, WIND, and CDATA are dimensioned by		*
C*        paramters.							*
C*									*
C* SNPDTW  ( IP1ARR, IP2ARR, IWNCOL, DATA, NLEV, IDTYPE, YSTRT, YSTOP,	*
C*           TEMP, NTEMP, DWPT, NDWPT, WIND, NWIND, XMIN, XMAX, IRET )	*
C*									*
C* Input parameters:							*
C*	IP1ARR (5)	INTEGER		Line info for parm 1		*
C*	IP2ARR (5)	INTEGER		Line info for parm 2		*
C*	IWNCOL		INTEGER		Color for wind			*
C*	DATA   (*)      REAL            Station data			*
C*	NLEV            INTEGER         Number of levels		*
C*	IDTYPE (*)	INTEGER		Level flags			*
C*					  1 = mandatory			*
C*					  2 = sig temp			*
C*					  3 = sig wind			*
C*	YSTRT		REAL		Bottom y			*
C*	YSTOP		REAL		Top y				*
C*	TEMP(NTEMP)     REAL		Temperatures			*
C*	NTEMP		INTEGER		Number of temperatures		*
C*	DWPT(NDWPT)	REAL		Dewpoint temperatures		*
C*	NDWPT		INTEGER		Number of dewpoint temperatures	*
C*	WIND(NWIND)	REAL		Winds				*
C*	NWIND		INTEGER		Number of winds			*
C*      XMIN		REAL		Minimum for x axis		*
C*	XMAX		REAL		Maximum for x axis		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/GSC		06/90	Created from first half of old SNPPLT	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		ip1arr (*), ip2arr (*), idtype (*)
	REAL		data (*)
	REAL            rdata (MMPARM),   wind (LLMXLV,3),
     +                  temp  (LLMXLV,2), dwpt (LLMXLV,2)
C*
	CHARACTER	cdata (MMPARM)*8
	LOGICAL		BETWEN
	INCLUDE		'ERMISS.FNC'
	BETWEN (ddd) = ( ( ddd .le. ystop ) .and. ( ddd .ge. ystrt ) )
     +					      .or.
     +			( ( ddd .ge. ystop ) .and. ( ddd .le. ystrt ) )
C*
C----------------------------------------------------------------------
	iret  = 0
	nwind = 0
	ntemp = 0
	ndwpt = 0
	xmin = 1.1e31
	xmax = -1.1e31
C
C*	Check that there is something to plot.
C
	IF  ( ( ip1arr (1) .eq. 0 ) .and. ( ip2arr (1) .eq. 0 ) .and.
     +	      ( iwncol .eq. 0 ) )  RETURN
C
C*	Loop through each level.
C
	DO  ilev = 1, nlev
C
C*	    Get data.
C
	    CALL PC_CMLV  ( ilev, data, rdata, cdata, ier )
C
C*	    Check for valid data in plotting range.
C
	    IF  ( ( .not. ERMISS ( rdata (1) ) ) .and. 
     +		  ( BETWEN ( rdata (1) ) ) )  THEN
C
C*		Get temperatures.
C
		IF  ( ( .not. ERMISS ( rdata (2) ) ) .and.
     +		      ( idtype (ilev) .ne. 3 ) )  THEN
		    ntemp = ntemp + 1
		    temp ( ntemp, 1 ) = rdata (1)
		    temp ( ntemp, 2 ) = rdata (2)
		    IF ( rdata (2) .gt. xmax ) xmax = rdata (2)
	  	    IF ( rdata (2) .lt. xmin ) xmin = rdata (2)
		END IF
C
C*		Get dewpoint.
C
		IF  ( ( .not. ERMISS ( rdata (3) ) ) .and. 
     +		      ( idtype (ilev) .ne. 3 ) )  THEN
		    ndwpt = ndwpt + 1
		    dwpt ( ndwpt, 1 ) = rdata (1)
		    dwpt ( ndwpt, 2 ) = rdata (3)
		    IF ( rdata (3) .gt. xmax ) xmax = rdata (3)
	  	    IF ( rdata (3) .lt. xmin ) xmin = rdata (3)
		END IF
C
C*		Get winds.
C
		IF  ( ( .not. ERMISS ( rdata (4) ) ) .and.
     +		      ( .not. ERMISS ( rdata (5) ) ) .and.
     +		      ( idtype (ilev) .ne. 2 ) )  THEN
		    nwind = nwind + 1
		    wind ( nwind, 1 ) = rdata (1)
		    wind ( nwind, 2 ) = rdata (4)
		    wind ( nwind, 3 ) = rdata (5)
		END IF
	    END IF
	END DO
C*
	RETURN
	END
