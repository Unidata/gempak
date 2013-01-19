	SUBROUTINE GDEGDT  ( lun, kx, ky, grid, iret )
C************************************************************************
C* GDEGDT								*
C*									*
C* This subroutine reads the data from a grid edit file.		*
C*									*
C* GDEGDT  ( LUN, KX, KY, GRID, IRET )					*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*									*
C* Output parameters:							*
C*	GRID (KX,KY)	REAL		Grid data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -6 = error reading data	*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* M. desJardins/GSFC	 9/88	Rewrote					*
C* S. Jacobs/NMC	 3/94	Updated checking for ROW header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  ( kx, ky )
C*
	CHARACTER	rec*132
	REAL		rarr (20)
	LOGICAL		done, row
C------------------------------------------------------------------------
	iret = 0
	done = .false.
	row  = .true.
	irow = ky
C
C*	Loop through processing records from edit file.
C
	DO WHILE ( .not. done )
C
C*	    Read record and convert to upper case.
C
	    READ  ( lun, 1000, IOSTAT = iostat )  rec
1000	    FORMAT ( A )
	    IF  ( iostat .ne. 0 )  THEN
		iret = -6
		RETURN
	    END IF
C
C*	    Skip blank lines.
C
	    IF  ( rec .ne. ' ' )  THEN
C
C*		If this line contains a row header, eliminate the word
C*		'ROW' and skip 1 column.
C
		IF  ( row )  THEN
		    CALL ST_LCUC ( rec, rec, ier )
C
C*		    Find 'ROW' and replace it with blanks.
C
		    ipos = INDEX ( rec, 'ROW' )
		    IF  ( ipos .ne. 0 )  THEN
			rec ( ipos : ipos+2 ) = '   '
		    END IF
		    istart = 2
		    icol   = 1
		  ELSE
		    istart = 1
		END IF
C
C*		Break record into real numbers.
C
		CALL ST_RLST  ( rec, ' ', RMISSD, 20,
     +				rarr, nreal, ier )
C
C*		Convert the character data to real numbers, and fill
C*		output array.
C
		DO  i = istart, nreal
		    IF  ( icol .le. kx )  THEN
			grid ( icol, irow ) = rarr (i)
		    END IF
		    icol = icol + 1
		END DO
	    END IF
C
C*	    Check for the end of this row.
C
	    IF  ( icol .gt. kx )  THEN
		row  = .true.
		irow = irow - 1
		IF  ( irow .eq. 0 )  done = .true.
	      ELSE
		row  = .false.
	    END IF
	END DO
C*
	RETURN
	END
