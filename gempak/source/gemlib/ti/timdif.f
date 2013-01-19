	SUBROUTINE TI_MDIF  ( idtar1, idtar2, nmin, iret )
C************************************************************************
C* TI_MDIF								*
C*									*
C* This subroutine computes the time difference in minutes between 	*
C* two integer times.  The time difference is time1 - time2 and		*
C* may be computed for a maximum of one year.				*
C*									*
C* TI_MDIF  ( IDTAR1, IDTAR2, NMIN, IRET )				*
C*									*
C* Input parameters:							*
C*	IDTAR1 (5)	INTEGER		Time array 1 (YYYY,MM,DD,HH,MM)	*
C*	IDTAR2 (5)	INTEGER		Time array 2 (YYYY,MM,DD,HH,MM)	*
C*									*
C* Output parameters:							*
C*	NMIN		INTEGER		Difference in minutes		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				  	-12 = invalid time range	*
C**									*
C* Log:									*
C* M. Goodman/RDS	12/85						*
C* M. desJardins/GSFC	11/87	GEMPAK 4				*
C* S. Jacobs/NCEP	 3/96	Fixed leap year calc involving Feb 29	*
C* T. Lee/GSC		 9/97	Fixed logical functions in special case	*
C* I. Durham/GSC	 9/98	Cleaned up				*
C* D. Kidwell/NCEP	 4/99	Fixed for Y2K                           *
C************************************************************************
	INTEGER		idtar1 (5), idtar2 (5)
C*
	INTEGER		jdtar1 (5), jdtar2 (5)
C*
	EQUIVALENCE	( jdtar1 (1), iyr1 ), ( jdtar2 (1), iyr2 ),
     +			( jdtar1 (2), imm1 ), ( jdtar2 (2), imm2 ),
     +			( jdtar1 (3), idd1 ), ( jdtar2 (3), idd2 ),
     +			( jdtar1 (4), ihh1 ), ( jdtar2 (4), ihh2 ),
     +			( jdtar1 (5), imin1), ( jdtar2 (5), imin2 )
C------------------------------------------------------------------------
	iret = 0
	nmin = 0
C
C*	Move times into arrays which are equivalenced to variable names.
C
	DO  i = 1, 5
	    jdtar1 (i) = idtar1 (i)
	    jdtar2 (i) = idtar2 (i)
	END DO
C
C*	Compute Julian days.
C
	CALL TI_ITOJ ( idtar1, jyear, jday1, ier )
	CALL TI_ITOJ ( idtar2, jyear, jday2, ier )
C
C*	Check that the year is four digits.
C
	CALL TI_YY24 ( iyr1, iyr1, ier1 )
	CALL TI_YY24 ( iyr2, iyr2, ier2 )
	IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) ) THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Add days for change in year.
C
	IF ( iyr1 .lt. iyr2 ) THEN
	   DO i = iyr1, iyr2-1
	      CALL TI_DAYM ( i, 2, ndays, ier )
	      jday2 = jday2 + 365
	      IF ( ndays .eq. 29 ) jday2 = jday2 + 1
	   END DO
	ELSE IF ( iyr1 .gt. iyr2 ) THEN
	   DO i = iyr2, iyr1-1
	      CALL TI_DAYM ( i, 2, ndays, ier )
	      jday1 = jday1 + 365
	      IF ( ndays .eq. 29 ) jday1 = jday1 + 1
	   END DO
	END IF
C
C*	Convert hours, minutes, and days to total minutes.
C
	nmin1 = jday1 * 1440 + ihh1 * 60 + imin1
	nmin2 = jday2 * 1440 + ihh2 * 60 + imin2
C
C*	Compute difference in minutes.
C
	nmin  = nmin1 - nmin2
C*
	RETURN
	END
