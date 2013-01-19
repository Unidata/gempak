	PROGRAM COUNTC
C************************************************************************
C* COUNTC								*
C*									*
C* This program counts C lines of code.					*
C*									*
C**									*
C* Log:									*
C* M. Linda/GSC		 2/97	Based on CNTCOD				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*128	fulpth, filnam, rec, lin, parm, carr(100)
	LOGICAL		optn, comflg
C------------------------------------------------------------------------
C
C*	Initialize variables.
C
	itotl = 0
	icode = 0
	icomm = 0
	iblnk = 0
C
	optn   = .false.
	comflg = .false.
C
C*	Get the arguments from the command line.
C
	narg = iargc ()
	IF  ( narg .lt. 1 )  THEN
	    WRITE (*,*) '...............................................'
	    WRITE (*,*) ''
	    WRITE (*,*) 'NAME     countc - C Lines of Code Counter'
	    WRITE (*,*) ''
	    WRITE (*,*) 'SYNOPSIS'
	    WRITE (*,*) '         countc [-n] path'
	    WRITE (*,*) ''
	    WRITE (*,*) 'DESCRIPTION'
	    WRITE (*,*) '         This utility counts lines of code '
	    WRITE (*,*) '         (LOC) for one file.  The file is '
	    WRITE (*,*) '         assumed to be C source.'
	    WRITE (*,*) ''
	    WRITE (*,*) '         -n    Suppress column labels.'
	    WRITE (*,*) ''
	    WRITE (*,*) 'SEE ALSO'
	    WRITE (*,*) '         cntall, cntloc, countf'
	    WRITE (*,*) '...............................................'
	    STOP
	ELSE
	    DO  i = 1, narg
		CALL GETARG ( i, parm )
C
		IF ( parm      .eq. '-n' ) optn = .true.
		IF ( parm(1:1) .ne. '-'  ) fulpth = parm
	    END DO
	END IF
C
C*	Open the file to be counted.
C
	CALL FL_SOPN ( fulpth, lunf, iopn )
C
C*	Read lines from the file until done.
C
	DO WHILE ( iopn .eq. 0 )
	    READ ( lunf, 1001, END = 100 ) rec
1001	    FORMAT ( A )
C
C*	    Increment the total line counter.
C
	    itotl = itotl + 1
C
C*	    Remove all white space.
C
	    CALL ST_RMBL ( rec, lin, lenl, iret )
C
C*	    Set a flag if a comment starts on this line and does not end.
C
	    ibeg = INDEX ( lin, '/*' )
	    iend = INDEX ( lin, '*/' )
	    IF ( ( ibeg .ne. 0 ) .and. ( iend .eq. 0 ) ) comflg = .true.
	    IF ( (   comflg    ) .and. ( iend .ne. 0 ) ) comflg = .false.
C
C*	    If the line is a blank line, increment counter.
C
	    IF  ( (   lin (1:3) .eq. '/**' ) .or.
     +		  (   lin (1:3) .eq. '/*-' ) .or.
     +		  (   lin (1:3) .eq. '***' ) .or.
     +		  ( ( lin (1:2) .eq. '**'  ) .and. ( lenl .eq. 2 ) ) .or.
     +		  ( ( lin (1:2) .eq. '/*'  ) .and. ( lenl .eq. 2 ) ) .or.
     +		  ( ( lin (1:2) .eq. '*/'  ) .and. ( lenl .eq. 2 ) ) .or.
     +		  (        lenl .le. 1     ) ) THEN
		iblnk = iblnk + 1
C
C*	    Else if the line is a comment, increment counter.
C
	    ELSE IF ( ( lin (1:2) .eq. '/*' ) .or. ( comflg ) ) THEN
		icomm = icomm + 1
	    ELSE
C
C*	    Else the line must be code, increment counter.
C
		icode = icode + 1
	    END IF
	END DO
100	CONTINUE
C
C*	Optionally display the column headers.
C
	IF  ( .not. optn ) THEN
	    WRITE (*,*) "     total      code   comment     blank"
	    WRITE (*,*) " --------- --------- --------- ---------"
	END IF
C
C*	Extract the file name out of the full path.
C
	CALL ST_CLST ( fulpth, '/', '', 100, carr, num, iret )
	filnam = carr (num)
	CALL ST_LSTR ( filnam, lenf, iret )
C
C*	Write out counters.
C
	IF ( iopn .eq. 0 ) THEN
	    WRITE (*,1000) itotl,icode,icomm,iblnk,filnam(1:lenf)
	END IF
1000	FORMAT ( " ", I10, I10, I10, I10, "  ", A )
C
C*	Close all files.
C
	CALL FL_CLAL ( iret )
C*
	END
