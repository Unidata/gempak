	SUBROUTINE SNDFLT ( numpts, fltarr, nprm, nparms, nlevel,
     +			    hdata, data, iret )
C************************************************************************
C* SNDFLT								*
C*									*
C* This routine will apply the requested filter to the wind and 	*
C* virtual potential temperature.					*
C*									*
C* SNDFLT ( NUMPTS, FLTARR, NPRM, NPARMS, NLEVEL, HDATA, DATA, IRET )	*
C*									*
C* Input parameters:							*
C*	NUMPTS		INTEGER		# points above and below level	*
C*	FLTARR 		REAL		Filter weights			*
C*	NPRM		INTEGER		Parameter to filter		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
C*	HDATA (LLMXLV)	REAL		Interpolated sounding data	*
C*									*
C* Output parameters:							*
C*	DATA (LLMXLV)	REAL		Filtered data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAXFLT = 20 )
C*
	REAL		fltarr(*), hdata(*), data(*)
C*
	REAL		tmparr(LLMXLV), arrim(LLMXLV), warr(MAXFLT),
     +			rsum(LLMXLV)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for missing data at the bottom of the sounding.
C
	i = 1
	DO  WHILE ( ( ERMISS(hdata((i-1)*nparms+nprm) ) ) .and.
     +		    ( i .lt. nlevel ) )
	    i = i + 1
	END DO
	istrt = i
C
C*	Check for missing data at the top of the sounding.
C
	i = nlevel
	DO  WHILE ( ( ERMISS(hdata((i-1)*nparms+nprm) ) ) .and.
     +		    ( i .gt. 1 ) )
	    i = i - 1
	END DO
	iend = i
C
C*	Fill the temporary array with the correct data.
C
	j = 0
	DO  i = istrt, iend
	    j = j + 1
	    tmparr(i-istrt+1) = hdata((i-1)*nparms+nprm)
	END DO
C
C*	Check for enough data.
C
	IF  ( j .lt. MAXFLT )  THEN
	    DO  i = 1, nlevel
		data(i) = RMISSD
	    END DO
	    iret = -1
	    RETURN
	END IF
C
C*	Add the weighting factors, and find the percentage
C*	contribution to the sum for each factor.
C
	wsum = fltarr(1)
	DO  i = 1, numpts
	    wsum = wsum + 2.*fltarr(i+1)
	END DO
	nmpt = numpts + 1
	DO  i = 1, nmpt
	    warr(i) = fltarr(i) / wsum
	END DO
	k = j
C
C*	Move the sounding up MAXFLT points. Put data in "image" array.
C
	DO  i = 1, k
	    arrim(j-i+MAXFLT+1) = tmparr(j-i+1)
	END DO
	j = j + MAXFLT
C
C*	Add data to each end of the sounding so that the filter can
C*	compute the necessary averages correctly.
C
	DO  i = 1, MAXFLT
	    arrim(0-i+MAXFLT+1) = 2.*arrim(MAXFLT+1) - arrim(i+MAXFLT+1)
	    arrim(0+i+j)        = 2.*arrim(j)        - arrim(0-i+j)
	END DO
C
C*	Find the sum of the multiplication of
C* 	the image array and percentage weights.
C
	DO  m = MAXFLT+1, j
	    sum = arrim(m) * warr(1)
	    DO  n = 1, numpts
		sum = sum + (arrim(m-n)+arrim(m+n))*warr(n+1)
	    END DO
	    rsum(m) = sum
	END DO
	j = j - MAXFLT
C
C*	If the original data had missing points on the ends, set the
C*	new array to missing for those same points.
C
	IF  ( istrt .gt. 1 )  THEN
	    DO  i = 1, istrt
		data(i) = RMISSD
	    END DO
	END IF
C
	IF  ( iend .lt. nlevel )  THEN
	    DO  i = iend, nlevel
		data(i) = RMISSD
	    END DO
	END IF
C
C*	Set the new array to the filtered values.
C
	data(istrt) = hdata((istrt-1)*nparms+nprm)
	DO  i = istrt+1, iend
	    data(i) = rsum(i+MAXFLT)
	END DO
C*
	RETURN
	END
