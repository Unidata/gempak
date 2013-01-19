	CHARACTER*(*) FUNCTION PT_VSBF  ( vsbf )
C************************************************************************
C* PT_VSBF								*
C*									*
C* This function converts the visibility in miles to a character string *
C* containing a fraction for visibilities under one mile. 		*
C*									*
C* CHARACTER*(*) PT_VSBF  ( VSBF )					*
C*									*
C* Input parameters:							*
C*	VSBF		REAL		Visibility			*
C*									*
C* Output parameters:							*
C*	PT_VSBF		CHAR*		Visibility with/without fraction*
C**									*
C* Log:									*
C* A. Hardy/GSC		4/99						*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   * 
C*                              DATA statement                          *
C* A. Hardy/SAIC	11/01   Reworked fractional comparison		*
C* A. Hardy/SAIC	12/01   Fixed missing fractional comparisons	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvalue*8, cvis(10)*4
	REAL            rvis (10)
C*
	INCLUDE		'ERMISS.FNC'
	DATA   rvis / 0.0625, 0.1250, 0.1875, 0.2500, 0.3125,
     +		      0.3750, 0.5000, 0.6250, 0.7500, 0.8750 /
        DATA   cvis / '1/16', '1/8', '3/16', '1/4', '5/16',
     + 		      '3/8', '1/2', '5/8', '3/4', '7/8' / 
C*
C------------------------------------------------------------------------
	PT_VSBF = ' '
C
C*	Check for missing data.
C
	IF  ( ERMISS ( vsbf ) ) THEN
            RETURN
C
C*	  Convert to character string.  If the visibility is
C*        equal or greater than 1, the real value is rounded
C*        to the closest integer.  If it is less than one
C*        the visibility is placed into a fraction. 
C
	  ELSE IF ( (vsbf .eq. 0. ) .or. ( vsbf .ge. 1.0 )  ) THEN
            ivsbf = NINT ( vsbf ) 
	    CALL ST_INCH ( ivsbf, cvalue, ier ) 
	    PT_VSBF =  cvalue
	  ELSE 
            DO ii = 1, 10
                IF ( ii .eq. 1 ) THEN
                    IF ( ( vsbf .lt. ( (rvis(ii+1) + rvis(ii+2) )/2.) )  
     +                     .and. ( vsbf .ge. 0.0300 ) )  THEN
      	                PT_VSBF =  cvis (ii)
                      ELSE IF ( ( vsbf .gt. 0.0) .and. 
     +                          ( vsbf .lt. 0.0300) ) THEN
      	                PT_VSBF =  ' '
                    END IF
                  ELSE IF ( ( ii .gt. 1 ) .and. (ii .lt. 10 )  )THEN
                    IF (( vsbf .ge. (( rvis(ii) + rvis(ii-1))/2.) ).and.
     +                 ( vsbf .le. (( rvis(ii) + rvis(ii+1))/2.) ) )THEN
      	                PT_VSBF =  cvis (ii)
                    END IF
                  ELSE IF ( ii .eq. 10 ) THEN
                    IF ( ( vsbf .gt. ( (rvis(ii-1) + rvis(ii))/2.) ) 
     +                   .and. ( vsbf .le. .8750 ) )  THEN
      	                PT_VSBF =  cvis (ii)
                      ELSE IF ( ( vsbf .gt. .8750 ) .and. 
     +                            (vsbf .le. .9999 ) )  THEN
      	                PT_VSBF =  '1'
                    END IF
                END IF
	    END DO
	END IF
C*
	RETURN
	END
