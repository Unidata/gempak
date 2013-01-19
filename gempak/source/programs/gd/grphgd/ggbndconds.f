	SUBROUTINE GGBNDCONDS ( kx, ky, iret )
C************************************************************************
C* GGBNDCONDS                                                           *
C*                                                                      *
C* This subroutine specifies boundary conditions for contour lines.	*
C*                                                                      *
C* GGBNDCONDS ( KX, KY, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*      KX              INTEGER         Data grid x-dimension           *
C*      KY              INTEGER         Data grid y-dimension           *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                                                     	*
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC		04/07	created					*
C* M. Li/SAIC		05/07	Check for valid boundary conditions	*
C* D.W.Plummer/NCEP	05/07	Some logic changes			*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'

	INTEGER 	iret, ier, kx, ky
C
	REAL            fiz(MAXPPL), fjz(MAXPPL)
	REAL            fbx(5,MAXLIN), fby(5,MAXLIN)
	INTEGER         X_MN, X_MX, Y_MN, Y_MX
	LOGICAL		valid_bnd
C-----------------------------------------------------------------------
C
	iret = 0
C
C* 	If user specified boudnary conds and an open line is encountered, 
C*	warn the user and return.
C
	IF ( ddist(1) .ne. IMISSD )  THEN
	  DO  nl = 1, nlines
	    if ( closed(nl) .ne. CLSD ) THEN
		iret = -15
		CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
		RETURN
	    END IF
	  END DO
        END IF
C
C*	Check for valid boundary conditions.
C
	valid_bnd = .false.
	if ( (ddist(1) .gt. 0 .and. ddist(2) .gt. ddist(1)) .or.
     +	     (ddist(1) .gt. 0 .and. ddist(2) .lt. 0) .or.
     +       (ddist(2) .gt. 0 .and. ddist(1) .lt. 0) )
     +	    valid_bnd = .true.     
C
C* 	If countours extend beyond the grid edge, reset visib to true
C
	X_MN = kx
        X_MX = 1
        Y_MN = ky
        Y_MX = 1

    	  DO  nl = 1, nlines
            CALL GTRANS ( 'M', 'G', npts(nl), flat(1,nl),
     +                    flon(1,nl), fiz, fjz, iret )
            DO  n = 1, npts(nl)
                X_MN = MIN ( X_MN, INT(fiz(n))   )
                X_MX = MAX ( X_MX, INT(fiz(n))+1 )
                Y_MN = MIN ( Y_MN, INT(fjz(n))   )
                Y_MX = MAX ( Y_MX, INT(fjz(n))+1 )
            END DO
          END DO
C
	IF ( valid_bnd .and. .not. visib ) THEN
	  IF ( X_MN .lt. 1 .or. Y_MN .lt. 1 .or.
     +	       X_MX .gt. kx .or. Y_MX .gt. ky ) THEN
	    visib = .true.
	    iret = 5
	    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
	  END IF
	END IF 

	IF ( spec .eq. 'e' ) THEN
	    X_MN = MIN (  1, X_MN )
	    X_MX = MAX ( kx, X_MX )
	    Y_MN = MIN (  1, Y_MN )
	    Y_MX = MAX ( ky, Y_MX )
	END IF
C	
C*    	Specify boundary conditions.
C
	  IF ( ddist(1) .gt. 0 .and. ddist(2) .gt. ddist(1) ) THEN
C
	    DO ii = 1, 2
	      nlines = nlines + 1
	      npts(nlines) = 5
	      value(nlines) = bvalue(ii)

	      fbx(ii, 1) = X_MN - ddist(ii)
	      fby(ii, 1) = Y_MN - ddist(ii)
	      fbx(ii, 2) = X_MX + ddist(ii)
              fby(ii, 2) = Y_MN - ddist(ii)
	      fbx(ii, 3) = X_MX + ddist(ii)
              fby(ii, 3) = Y_MX + ddist(ii)
	      fbx(ii, 4) = X_MN - ddist(ii)
              fby(ii, 4) = Y_MX + ddist(ii)
	      fbx(ii, 5) = X_MN - ddist(ii)
              fby(ii, 5) = Y_MN - ddist(ii)
	    END DO

	    DO ii = 1, 5
	      DO nl = nlines-1, nlines
		jj = nl - nlines + 2
		CALL GTRANS ( 'G', 'M', 1, fbx(jj,ii), fby(jj,ii),
     +			      flat(ii, nl), flon(ii,nl), ier )
	      END DO
	    END DO
C*
	  ELSE IF ( ddist(1) .gt. 0 .and. ddist(2) .lt. 0 .or.
     +              ddist(2) .gt. 0 .and. ddist(1) .lt. 0 ) THEN

	    IF ( ddist(1) .gt. 0 .and. ddist(2) .lt. 0 ) THEN
	      ij = 1
	    ELSE
	      ij = 2
	    END IF
C
	    nlines = nlines + 1
            npts(nlines) = 5
            value(nlines) = bvalue(ij)

            fbx(ij, 1) = X_MN - ddist(ij)
            fby(ij, 1) = Y_MN - ddist(ij)
            fbx(ij, 2) = X_MX + ddist(ij)
            fby(ij, 2) = Y_MN - ddist(ij)
            fbx(ij, 3) = X_MX + ddist(ij)
            fby(ij, 3) = Y_MX + ddist(ij)
            fbx(ij, 4) = X_MN - ddist(ij)
            fby(ij, 4) = Y_MX + ddist(ij)
            fbx(ij, 5) = X_MN - ddist(ij)
            fby(ij, 5) = Y_MN - ddist(ij)
C
	    DO ii = 1, 5
	      CALL GTRANS ( 'G', 'M', 1, fbx(ij,ii), fby(ij,ii),
     +                     flat(ii, nlines), flon(ii,nlines), ier )
	    END DO
C*
	  END IF
C*
	RETURN
	END
