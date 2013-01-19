	PROGRAM NAMCHECK
C************************************************************************
C* NAMCHECK								*
C*									*
C* This programs finds records which cross the IDL.			*
C**									*
C* Log:									*
C* M. desJardins/NMC	10/91						*
C************************************************************************
	REAL		rlat (100), rlon (100)
	CHARACTER	name*64, infile*80
	LOGICAL		good, done
C------------------------------------------------------------------------
C*	Initialize GEMPAK.
C
	CALL IN_BDTA ( ier )
        WRITE  ( 6, * ) 'Enter NAME map file to be checked: '
	READ   ( 5, 2 )  infile
2	FORMAT ( A ) 
C
C*	Open the files and check for errors.
C
	CALL FL_SOPN  ( infile, lunin, ier1 )
	IF  ( ier1 .ne. 0 )  THEN
            CALL ER_WMSG  ( 'FL', ier1, infile, ier )
            CALL FL_CLOS  ( lunin, ier )
            STOP
        END IF
C
C*	Loop through data in file.
C
	done   = .false.
	nptold = 1
	DO WHILE  ( .not. done )
C
C*	  Read header record for this buffer.
C
	  READ  ( lunin, 1100, IOSTAT = iostat )  npts, nrec, name
1100	  FORMAT ( I3, 2X, I5, 4X, A )
	  IF  ( iostat .ne. 0 )  THEN
	   done = .true.
	  ELSE 
C
C*	    If npts is negative, this is a continuation.  Therefore,
C*	    we must get the last point from the last record.
C
	    IF  ( npts .lt. 0 )  THEN
		npts = -npts
		ipt1 = 2
		ipt2 = npts + 1
		rlat (1) = rlat ( nptold )
		rlon (1) = rlon ( nptold )
	      ELSE
		ipt1 = 1
		ipt2 = npts
	    END IF
C
C*	    Read in the lat/lon pairs.
C
	    READ   ( lunin, 1200 ) ( rlat (i), rlon (i), i = ipt1, ipt2 )
1200	    FORMAT ( 8F10.5 )
C
C*	    Check for IDL crossing.
C
	    good = .true.
	    DO  iii = 2, npts
		IF  ( rlon (iii) * rlon (iii-1) .lt. 0. )  THEN
		    IF  ( ABS ( rlon (iii) ) .gt. 170. )  good = .false.
		END IF
	    END DO
	    IF  ( .not. good )  THEN
		WRITE (6,*) 'nrec = ', nrec
	    END IF
C
C*	    Save variable names.
C
	    nptold = ipt2
	  END IF
	END DO
C*
	END
