	SUBROUTINE G2T_RANGE ( ktype, mval, irange, iret )
C************************************************************************
C* G2T_RANGE 								*
C*									*
C* This subroutine returns the range for a give wave or wind value.	*
C*									*
C* G2T_RANGE ( KTYPE, MVAL, IRANGE, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	MVAL		INTEGER		Maximum wave height/wind	*
C*									*
C* Output parameters:							*
C*	IRANGE		INTEGER		Maxium range			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C* T. Lee/SAIC		11/07	Use g2t_parm.tbl to get range		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	LOGICAL		contin
	INCLUDE 	'EQUAL.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
	IF ( IRMISS ( mval ) )  THEN
	    irange = 0
	    RETURN
	END IF
C
C*	Wave heights.
C
	IF  ( ktype .eq. 1 )  THEN
	    IF ( ncnter .eq. 2 )  THEN
		irange = NINT ( mval / 3. )
	      ELSE
		kw = 1
		contin = .true.
		DO WHILE ( contin )
		    IF ( mval .ge. mnwgap ( kw, 1  )  .and.
     +			 mval .lt. mnwgap ( kw, 2  ) )  THEN
			contin = .false.
			irange = mnwgap ( kw, 3 )
		    END IF
		    kw = kw + 1
		    IF ( kw .gt. llwgap ) contin = .false.
		END DO
	    END IF
	  ELSE IF ( ktype .eq. 2 )  THEN
	    ks = 1
	    contin = .true.
	    DO WHILE ( contin )
		IF ( mval .gt. mnsgap ( ks, 1  )  .and.
     +		     mval .le. mnsgap ( ks, 2  ) )  THEN
		    contin = .false.
		    irange = mnsgap ( ks, 3 )
		END IF
		ks = ks + 1
		IF ( ks .gt. llsgap ) contin = .false.
	    END DO
	END IF
C*		
	RETURN
	END
