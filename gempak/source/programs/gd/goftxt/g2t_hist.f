	SUBROUTINE G2T_HIST ( nt, nz, wok, sok, iret )
C************************************************************************
C* G2T_HIST								*
C*									*
C* This subroutine computes wave and/or wind ranges based on histograms *
C* if grid values exceed the allowable ranges.	The result will be	*
C* stored in the common.						* 
C*									*
C* G2T_HIST ( NT, NZ, WOK, SOK, IRET )					*
C*									*
C* Input and output parameters:						*
C*	NT		INTEGER		Nth time step			*
C*	NZ		INTEGER		Nth zone area			*
C*	WOK		LOGICAL		Wave range OK flag		*
C*	SOK		LOGICAL		Wind range OK flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		11/06						*
C* T. Lee/SAIC		10/07	Set zone flag, MNZFLG, for min values	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	LOGICAL		wok, sok
	LOGICAL		ok, dum
	INTEGER		kwave (2), kwind (2)
	INCLUDE		'EQUAL.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Wave.
C
	IF  ( .not. wok )  THEN
C
C*	    Get number of zones generate max wave heights.
C
	    nb = 0
	    jtemp = mxval ( 1, 1, nt )
	    DO ii = 2, nsubzn ( nz )
		IF ( mxval ( 1, 1, nt ) .eq. mxval ( 1, ii, nt ) ) THEN
		    nb = nb + 1
		END IF
		IF ( .not. IRMISS ( mxval ( 1, ii, nt ) ) )  THEN
		    IF ( mxval ( 1, ii, nt ) .lt. jtemp )  THEN
			jtemp = mxval ( 1, ii, nt ) 
		    END IF
		END IF
	    END DO
C
C*	    Multiple zones: wave range = ( mxval - range, mxval ).
C*	    Store the range over the 1st zone.
C
	    IF ( nb .gt. 1 )  THEN
		CALL G2T_RANGE ( 1, mxval ( 1, 1, nt ), jrange, ier )
		itemp = mxval ( 1, 1, nt ) - jrange
		mnval ( 1, 1,nt ) = AMAX0 ( itemp, mnval ( 1, 1, nt ) )
		wok = .true.
	    END IF
C
C*	    If min wave height occurs over one zone, set MNZFLG so
C*	    that the min wave height will be set in the EXCEPT part.
C
	    mb = 0
	    DO ii = 2, nsubzn ( nz )
		IF ( jtemp .eq. mxval ( 1, ii, nt ) ) THEN
		    mb = mb + 1
		END IF
	    END DO
	    IF ( mb .eq. 1 )  mnzflg ( 1 ) = .true.
C
C*	    Single subzone: wave range from histograms.
C
	    IF  ( .not. wok )  THEN
	        CALL G2T_SORT ( 1, nz, iret )
		IF ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		    RETURN
		END IF
	        jj = 2
	        ok = .true.
	        psum = wavpct ( 1, nt, 1 )
		jmax = icwave ( 1, nt, 1 )
		jmin = icwave ( 1, nt, 1 )
	        kwave ( 1 ) = icwave ( 1, nt, 1 )
	        kwave ( 2 ) = icwave ( 1, nt, 1 )
	        DO WHILE ( ok )
		    IF ( icwave ( 1, nt, jj ) .gt. jmax )  THEN
			jmax = icwave ( 1, nt, jj )
			kwave ( 1 ) = jmax
		      ELSE IF ( icwave ( 1, nt, jj ) .lt. jmin )  THEN
			jmin = icwave ( 1, nt, jj )
			kwave ( 2 ) = jmin
		    END IF
		    CALL G2T_CHECK ( 1, kwave, kwave, ok, dum, ier )
		    IF ( ok )  THEN
		        jj = jj + 1
		        IF  ( wavpct ( 1, nt, jj ) .gt. 0. )  THEN
		    	    psum = psum + wavpct ( 1, nt, jj )
		          ELSE
			    ok = .false.
			    jj = jj - 1
		        END IF
		    END IF
	        END DO
C
C*	        Sort out final wave heights and store in common.
C
		DO kx = 1, jj - 1
		    DO ky = kx + 1,jj 
			IF ( icwave ( 1, nt, ky ) .gt.
     +			     icwave ( 1, nt, kx ) )  THEN
			    iw1 = icwave ( 1, nt, ky )
			    iw2 = icwave ( 1, nt, kx )
			    icwave ( 1, nt, ky ) = iw2
			    icwave ( 1, nt, kx ) = iw1
			END IF
		    END DO
	        END DO
		mxval ( 1, 1, nt ) = icwave ( 1, nt, 1 )
	        CALL G2T_RANGE ( 1, mxval (1,1,nt), jrange, iret )
		jtemp = mxval ( 1, 1, nt ) - jrange
	        IF ( jj .eq. 1 )  THEN
		    mnval ( 1,1,nt ) = AMAX0 ( mnval_s (1,1,nt),jtemp )
	          ELSE
		    mnval ( 1,1,nt ) = AMAX0 ( icwave (1,nt,jj),jtemp )
	        END IF
	        wok = .true.
	    END IF
	END IF
C
C*	Sort order wind frequency distribution in percentage.
C
	IF ( .not. sok )  THEN
	    nb = 0
	    jtemp = mxval ( 2, 1, nt )
C
C*	    Get number of zones generate wind max.
C
	    DO ii = 2, nsubzn ( nz )
		IF ( mxval ( 2, 1, nt ) .eq. mxval ( 2, ii, nt ) ) THEN
		    nb = nb + 1
		END IF
		IF ( .not. IRMISS ( mxval ( 2, ii, nt ) ) )  THEN
		    IF ( mxval ( 2, ii, nt ) .lt. jtemp )  THEN
			jtemp = mxval ( 2, ii, nt )
		    END IF
		END IF
	    END DO
C
C*	    Multiple zones: wind range = ( mxwind - range, mxwind ).
C*	    Store the range over the 1st zone.
C
	    IF ( nb .gt. 1 )  THEN
		CALL G2T_RANGE ( 2, mxval ( 2,1,nt ), krange, ier )
		itemp = mxval ( 2, 1, nt ) - krange
		mnval ( 2, 1, nt ) = AMAX0 ( itemp, mnval ( 2, 1, nt ) )
		sok = .true.
	    END IF
C
C*          If min wind occurs over one zone, set MNZFLG so that the wind info
C*	    will be set in the EXCEPT part.
C
	    mb = 0
	    DO ii = 2, nsubzn ( nz )
		IF ( jtemp .eq. mxval ( 2, ii, nt ) ) THEN
		    mb = mb + 1
		END IF
	    END DO
            IF ( mb .eq. 1 )  mnzflg ( 2 ) = .true.
C
	    IF  ( .not. sok )  THEN
	        CALL G2T_SORT ( 2, nz, iret )
		IF ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		    RETURN
		END IF
C
C*	        Single subzone: speed range from histograms.
C
	        kk = 2
	        ok = .true.
	        psum = winpct ( 1, nt, 1 )
	        kmax = icwind ( 1, nt, 1 )
	        kmin = icwind ( 1, nt, 1 )
	        kwind ( 1 ) = ( kmax - 1 ) * 5
	        kwind ( 2 ) = ( kmin - 1 ) * 5
	        DO WHILE ( ok )
		    IF ( icwind ( 1, nt, kk ) .gt. kmax )  THEN
		        kmax =  icwind ( 1, nt, kk )
		        kwind ( 1 ) = ( kmax - 1 ) * 5
		      ELSE IF ( icwind ( 1, nt, kk ) .le. kmin )  THEN
		        kmin = icwind ( 1, nt, kk )	
		        kwind ( 2 ) = ( kmin - 1 ) * 5
		    END IF
C
		    CALL G2T_CHECK ( 2, kwind, kwind, dum, ok , ier )
		    IF  ( ok )  THEN
		        kk = kk + 1
		        IF  ( winpct ( 1, nt, kk ) .gt. 0. )  THEN
			    psum = psum + winpct ( 1, nt, kk )
		          ELSE
			    kk = kk - 1
			    ok = .false.
		        END IF
		    END IF
	        END DO
C
C*	        Sort out final wind speed and store in common.
C
	        DO kx = 1, kk - 1
		    DO ky = kx + 1, kk 
		        IF ( icwind ( 1, nt, ky ) .gt.
     +			     icwind ( 1, nt, kx ) )  THEN
			    b1 = icwind ( 1, nt, ky )
			    b2 = icwind ( 1, nt, kx )
			    icwind ( 1, nt, ky ) = b2
			    icwind ( 1, nt, kx ) = b1
		        END IF
		    END DO
	        END DO
C
	        mxval ( 2, 1, nt ) = ( icwind ( 1, nt, 1 ) - 1 ) * 5
	        CALL G2T_RANGE ( 2, mxval ( 2, 1, nt ), krange, iret )
		ktemp = mxval ( 2, 1, nt ) - krange
	        IF ( kk .eq. 1 )  THEN
		    mnval ( 2, 1, nt ) = AMAX0 ( ktemp,mnval_s(2,1,nt) )
	          ELSE
		    mnval ( 2, 1, nt ) = 
     +			AMAX0 ( ktemp, ( icwind (1, nt, kk) - 1 ) * 5 )
	        END IF
	        sok = .true.
	    END IF
	END IF
C*
	RETURN
	END
