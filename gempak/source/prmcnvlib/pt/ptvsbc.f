	CHARACTER*(*) FUNCTION PT_VSBC  ( vsby )
C************************************************************************
C* PT_VSBC								*
C*									*
C* This function converts the visibility in miles to a character string *
C* containing a fraction for visibilities under one mile. 		*
C*									*
C* CHARACTER*(*) PT_VSBC ( VSBY )					*
C*									*
C* Input parameters:							*
C*	VSBY		REAL		Visibility			*
C*									*
C* Output parameters:							*
C*	PT_VSBC		CHAR*		Visibility with fraction	*
C**									*
C* Log:									*
C* A. Hardy/SAIC	11/01						*
C* A. Hardy/SAIC	12/01	Added check for whole fractional number	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvis*12, cnum*12
	CHARACTER	pt_vsbf*12	
C*
	INCLUDE		'ERMISS.FNC'
C*
C------------------------------------------------------------------------
	PT_VSBC = ' '
C
C*	Check for missing data.
C
	IF  ( ERMISS ( vsby ) ) THEN
            RETURN
C
C*	  Convert to character string. 
C
          ELSE
            ivis = INT ( vsby )
            CALL ST_INCH ( ivis, cnum, ier )
            CALL ST_LSTR ( cnum, len, ier )
            frac = vsby - INT ( vsby )
C
C*          If the fraction is equal to '0' or if the fraction is a very,
C*          very, small number close to '0', return the whole number.
C
            IF ( ( frac .eq. 0.0 ) .or. ( frac .lt. 0.0600 ) ) THEN
                PT_VSBC =  cnum(:len) 
              ELSE
C
C*              Return the fraction.
C
                cvis = PT_VSBF ( frac )
                IF ( ivis .lt. 1 )  THEN
                    PT_VSBC =  cvis 
                  ELSE 
C
C*                  Return the whole number with the fraction.
C
                    IF ( cvis .ne. ' ' )  THEN
                        IF ( cvis .ne. '1') THEN
                            PT_VSBC =  cnum(:len) // '-' // cvis 
                          ELSE
                            ivis = ivis + 1
                            CALL ST_INCH ( ivis, cnum, ier )
                            CALL ST_LSTR ( cnum, len, ier )
                            PT_VSBC =  cnum(:len)
                        END IF
                      ELSE
C
C*                      Return the whole number without the fraction.
C*                      because the fractional part was not found in
C*                      PT_VSBF.
C
                        PT_VSBC =  cnum(:len) 
                    END IF
                END IF
            END IF
	END IF
C*
	RETURN
	END
