	PROGRAM SSFNAM
C************************************************************************
C* SSFNAM								*
C*									*
C* This program converts a SSF file into a NAME file which is in a	*
C* different format and which contains geographic names for each set	*
C* of lat/lon points.							*
C**									*
C* Log:									*
C* M. desJardins/NMC	10/91						*
C************************************************************************
	REAL		rlat (500), rlon (500)
	CHARACTER	infile*80, outfil*80
	LOGICAL		done, more
C------------------------------------------------------------------------
C*	Initialize GEMPAK.
C
	CALL IN_BDTA ( ier )
        WRITE  ( 6, * ) 'Enter SSF map file to be converted: '
	READ   ( 5, 2 )  infile
2	FORMAT ( A ) 
        WRITE  ( 6, * ) 'Enter name of NAM map file to be created: '
	READ   ( 5, 2 )  outfil
C
C*	Open the files and check for errors.
C
	CALL FL_SOPN  ( infile, lunin, ier1 )
	CALL FL_SWOP  ( outfil, lunot, ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
            CALL ER_WMSG  ( 'FL', ier1, infile, ier )
            CALL ER_WMSG  ( 'FL', ier2, outfil, ier )
            CALL FL_CLOS  ( lunin, ier )
            CALL FL_CLOS  ( lunot, ier )
            STOP
        END IF
C
C*	Loop through data in file.
C
	nptold = 0
	done   = .false.
	nrec   = 0
	DO WHILE  ( .not. done )
C
C*	  Read header record for this buffer.
C
	  READ  ( lunin, 1400, IOSTAT = iostat ) 
     +			iout, rltmx, rltmn, rlnmx, rlnmn,
     +		       ( rlat (k), rlon (k), k = 1, iout/2 )
1400	  FORMAT ( I4, 14X, 6F9.3, 8X, / ( 8F9.3, 8X ) )
	  iout = iout / 2
	  write (6,*) 'iout = ', iout, rlat (1), rlon (1)
C
C*	  Break record into length of 48 or less.
C
	  IF  ( iostat .eq. 0 )  THEN
	    done = .false.
	    more = .true.
	  ELSE
	    done = .true.
	    more = .false.
	  END IF
	  is = 1
C
C*	  Get next buffer.
C
	  DO WHILE  ( more )
C*
	    ie  = is + 47
	    IF  ( ie .gt. iout )  THEN
	        ie = iout
	    END IF
	    ipt = ie - is + 1
C
C*	    Check for a continuation.
C
	    IF  ( is .ne. 1 )  ipt = - ipt
C
C*	    Write out the record.
C
	    nrec = nrec + 1
	    WRITE  ( lunot, 1100, IOSTAT = iostat )  
     +					ipt, nrec
1100	    FORMAT ( I3, 2X, I5, 4X )
	    WRITE  ( lunot, 1200 ) 
     +			( rlat (i), rlon (i), i = is, ie )
1200	    FORMAT ( 8F10.5 )
C
C*	    Prepare for next record.
C
	    is = ie + 1
	    IF  ( is .gt. iout )  more = .false.
	  END DO
	END DO
C*
	END
