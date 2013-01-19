	SUBROUTINE GH_SPRL ( grid, imax, jmax, ibeg, iend, jbeg, jend,
     +			     iret )
C************************************************************************
C* GH_SPRL 								*
C*									*
C* This subroutine finds the beginning and ending columns and rows      *
C* which contain values less than the minimum contour threshold of 10.  *
C*									*
C* GH_SPRL ( GRID, IMAX, JMAX, IBEG, IEND, JBEG, JEND, IRET )		*
C*									*
C* Input parameters:							*
C*	GRID(IMAX,JMAX) REAL		Grid data array			*
C*      IMAX		INTEGER		Maximum number of rows		*
C*      JMAX		INTEGER		Maximum number of columns	*
C*									*
C* Output parameters:							*
C*	IBEG		INTEGER		Beginning row			*
C*	IEND		INTEGER		Ending row			*
C*	JBEG		INTEGER	        Beginning column		*
C*	JEND		INTEGER		Ending column			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return            *
C*					   1 = contourable data >=58 deg*
C*					  -2 = no contourable data      *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	4/01 						*
C* D. Kidwell/NCEP	6/01 	Added return code value of 1		*
C* D. Kidwell/NCEP	4/02 	Changed to check for contour minimum    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        REAL		grid (imax,jmax) 
C*
	LOGICAL		done
C-----------------------------------------------------------------------
	iret = -2
	cmin = 10.
C
C*	Find the first non-zero column.
C
	done = .false.
	i    = 1
	DO WHILE ( .not. done )
	    DO j = 1, jmax
		IF ( grid ( i, j ) .gt. cmin ) done = .true.
	    END DO
	    IF ( done .or. ( i .eq. imax ) ) THEN
		ibeg = i
		done = .true.
	      ELSE
		i = i + 1
	    END IF
	END DO
C
C*	Find the last non-zero column.
C
	done = .false.
	i    = imax
	DO WHILE ( .not. done )
	    DO j = 1, jmax
		IF ( grid ( i, j ) .gt. cmin ) done = .true.
	    END DO
	    IF ( done .or. ( i .eq. 1 ) ) THEN
		iend = i
		done = .true.
	      ELSE
		i = i - 1
	    END IF
	END DO
C
C*	Find the first non-zero row.
C
	done = .false.
	j    = 1
	DO WHILE ( .not. done )
	    DO i = 1, imax
		IF ( grid ( i, j ) .gt. cmin ) done = .true.
	    END DO
	    IF ( done .or. ( j .eq. jmax ) ) THEN
		jbeg = j
		done = .true.
	      ELSE
		j = j + 1
	    END IF
	END DO
C
C*	Find the last non-zero row.
C
	done = .false.
	j    = jmax
	DO WHILE ( .not. done )
	    DO i = 1, imax
		IF ( grid ( i, j ) .gt. cmin ) done = .true.
	    END DO
	    IF ( done .or. ( j .eq. 1 ) ) THEN
		jend = j
		done = .true.
	      ELSE
		j = j - 1
	    END IF
	END DO
C
C*	Check for contourable data and value above 58 N.
C
	IF ( ( ibeg .le. iend ) .and. ( jbeg .le. jend ) ) THEN
	    iret = 0
	    IF ( jend .ge. ( jmax - 2 ) ) iret = 1
	END IF
C*
	RETURN
	END
