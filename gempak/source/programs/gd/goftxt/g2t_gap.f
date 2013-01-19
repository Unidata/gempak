	SUBROUTINE G2T_GAP ( ktype, iret )
C************************************************************************
C* G2T_GAP								*
C*									*
C* This subroutine eliminates gaps between maximum range and minimum    *
C* exception.								*
C*									*
C* G2T_GAP ( KTYPE, IRET )						*
C*									*
C* Input and output parameters:						*
C*	WD(2)		CHAR*		First wind direction		*
C*									*
C* Output Parameters:							*
C*	SPREAD		LOGICAL		Wind spread flag		*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		12/07						*
C* T. Lee/SAIC		12/07	Table driven gap values			*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	INTEGER		kgap(2,2)
	LOGICAL		contin
	INCLUDE		'EQUAL.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*      Handle Range/Except gap.
C
	IF ( ktype .eq. 1 )  THEN
	    DO it = 1, 2
		mval = AMAX0 ( mxn_d (ktype,it,2), mxn_de (ktype,it,2) )
		IF ( ncnter .eq. 2 .and. .not. IRMISS (mval) )  THEN
		    kgap ( ktype, it ) = IFIX ( mval / 3. ) 
		  ELSE
		    kw = 1
		    contin = .true.
		    DO WHILE ( contin )
			IF ( mval .ge. mnwgap ( kw, 1 ) .and.
     +			     mval .lt. mnwgap ( kw, 2 ) )  THEN
			    contin = .false.
			    kgap ( ktype, it ) = mnwgap ( kw,4 )
			END IF
			kw = kw + 1
			IF ( kw .gt. llwgap ) contin = .false.
		    END DO
		END IF
	    END DO
	  ELSE
	    DO it = 1, 2
		mval = AMAX0 ( mxn_d (ktype,it,2), mxn_de (ktype,it,2) )
		ks = 1
		contin = .true.
		DO WHILE ( contin )
		    IF ( mval .gt. mnsgap ( ks, 1 ) .and.
     +			 mval .le. mnsgap ( ks, 2 ) )  THEN
			contin = .false.
			kgap ( ktype, it ) = mnsgap ( ks, 4 )
		    END IF 
		    ks = ks + 1
		    IF ( ks .gt. llsgap ) contin = .false.
		END DO
	    END DO
	END IF
C
	DO itrnd = 1,  2
	    IF ( eflag_d ( ktype, itrnd ) )  THEN
		maxr = mxn_d  ( ktype, itrnd, 2 )
		minr = mxn_d  ( ktype, itrnd, 1 )
		mine = mxn_de ( ktype, itrnd, 1 )
		mdif = mine - maxr
		IF ( mdif .gt. kgap ( ktype, itrnd ) )  THEN
		    maxr = mine - kgap ( ktype, itrnd )
		    CALL G2T_RANGE ( ktype, maxr, irange, ier )
		    IF ( ABS ( maxr - minr ) .gt. irange )
     +		        minr = maxr - irange
		    mxn_d ( ktype, itrnd, 2 ) = maxr
		    mxn_d ( ktype, itrnd, 1 ) = minr
		END IF
	    END IF
	END DO
C*
	RETURN
	END
