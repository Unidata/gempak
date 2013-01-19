	SUBROUTINE BFRMRG ( np, nz2, sndat2, nz, sndata, iret )
C************************************************************************
C* BFRMRG								*
C*									*
C* This subroutine performs a software merge of new sounding data	*
C* with data already stored.  A cross check of the data is done to	*
C* locate and replace missing values that correspond in position to	*
C* non-missing values.							*
C*									*
C* Assumption:  First parameter is pressure and is used for sorting.	*
C*									*
C* BFRMRG ( NP, NZ2, SNDAT2, NZ, SNDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	NP		INTGER		Number of parameters		*
C*	NZ2		INTEGER		Number of SNDAT2 levels		*
C*	SNDAT2 (NP,NZ2)	REAL		Read-back sounding data		*
C*									*
C* Input and Output parameter:						*
C*	NZ		INTEGER		Input:  # levels in new snd	*
C*					Output:  # levels in merged snd *
C*	SNDATA (NP,NZ)	REAL		Input:  newly read sounding data*
C*					Output:  merged sounding data	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/01						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		sndat2 (np,*), sndata (np,*)
C*
	LOGICAL		found
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C*
	iadd = nz
	DO k2 = 1, nz2
	    IF ( .not. ERMISS ( sndat2 (1,k2) ) ) THEN
		found = .false.
		k = 0
		DO WHILE ( k .lt. nz .and. .not. found )
		    k = k + 1
		    IF ( ABS ( sndat2 (1,k2) - sndata (1,k) ) .lt.
     +			.005 ) THEN
			found = .true.
			DO ip = 2, np
			    IF ( ERMISS ( sndata (ip,k) ) ) THEN
				sndata (ip,k) = sndat2 (ip,k2)
			    END IF
			END DO
		    END IF
		END DO
		IF ( .not. found ) THEN
		    iadd = iadd + 1
		    DO ip = 1, np
			sndata (ip,iadd) = sndat2 (ip,k2)
		    END DO
		END IF
	    END IF
	END DO
C
C*	Sort the pressure data.
C
	nz = iadd
	DO i = 1, nz
	    DO j = 1, nz-1
		IF ( sndata (1,j) .lt. sndata (1,j+1) ) THEN
		    DO ip = 1, np
			save = sndata (ip,j)
			sndata (ip,j) = sndata (ip,j+1)
			sndata (ip,j+1) = save
		    END DO
		END IF
	    END DO
	END DO
C
C*	Eliminate duplicate levels.
C
	nzo = nz-1
	DO i = 1, nzo
	    IF ( sndata (1,i) .eq. sndata (1,i+1) ) THEN
		DO ip = 2, np
		    IF ( ERMISS ( sndata (ip,i) ) ) THEN
			sndata (ip,i) = sndata (ip,i+1)
		    END IF
		END DO
		IF ( i+1 .ne. nz ) THEN
		    nz = nz - 1
		    DO j = i+1, nz
			DO ip = 1, np
			    sndata (ip,j) = sndata (ip,j+1)
			END DO
		    END DO
		ELSE
		    nz = nz - 1
		END IF
	    END IF
	END DO
C
C*	Eliminate zero or negative pressure levels.
C
	i = 0
	DO WHILE ( i .lt. nz )
	    i = i + 1
	    IF ( sndata (1,i) .le. 0.0 ) THEN
		nz = i - 1
		i = nz + 1
	    END IF
	END DO
C*
	RETURN
	END
