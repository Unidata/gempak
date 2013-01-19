	SUBROUTINE GDEGHD  ( lun, ncol, nrow, time, level, ivcord, parm,
     +			     iscale, iret )
C************************************************************************
C* GDEGHD								*
C*									*
C* This subroutine reads the header information from a grid edit file.	*
C*									*
C* GDEGHD  ( LUN, NCOL, NROW, TIME, LEVEL, IVCORD, PARM, ISCALE, IRET )	*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*									*
C* Output parameters:							*
C*	NCOL		INTEGER		Grid y dimension		*
C*	NROW		INTEGER		Grid x dimension		*
C*	TIME  (2)	CHAR*		Grid date/times			*
C*	LEVEL (2)	INTEGER		Grid levels			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*		Grid name			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = end of file read		*
C*					 -3 = no grid identifier	*
C*					 -4 = no grid size		*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* M. desJardins/GSFC	 9/88	Rewrote for GEMPAK4			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	time  (2), parm
	INTEGER		level (*)
C*
	CHARACTER	rec*132
	LOGICAL		done, idnfnd, sizfnd
C------------------------------------------------------------------------
	idnfnd = .false.
	sizfnd = .false.
	done   = .false.
	iscale = 0
	iret   = 0
C
C*	Loop through processing records from edit file.
C
	DO WHILE  ( .not. done )
C
C*	    Read record and convert to upper case.
C
	    READ   ( lun, 1000, IOSTAT = iostat )  rec
1000	    FORMAT ( A )
	    IF  ( iostat .ne. 0 )  THEN
		iret = -1
		RETURN
	    END IF
	    CALL ST_LCUC  ( rec, rec, ier )
C
C*	    Search for keywords.
C
	    islash = INDEX  ( rec, '/' )
	    icol   = INDEX  ( rec, 'COLUMNS:' )
	    irow   = INDEX  ( rec, 'ROWS:' )
	    ifac   = INDEX  ( rec, 'FACTOR' )
	    idata  = INDEX  ( rec, 'ROW' )
C
C*	    If slash for time found, get the time, level, vertical 
C*	    coordinate, and parameter.
C
	    IF  ( islash .gt. 0 )  THEN
		CALL GDEIDN  ( rec, time, level, ivcord, parm, ier )
		IF  ( ier .eq. 0 )  idnfnd = .true.
C
C*		Find start and end row and column.
C
	      ELSE IF  ( ( icol .gt. 0 ) .and. ( irow .gt. 0 ) )  THEN
		CALL GDESIZ  ( rec, ncol, nrow, ier )
		IF  ( iret .eq. 0 )  THEN
		    sizfnd = .true.
		  ELSE
		    RETURN
		END IF
C
C*		Get scaling factor.
C
	      ELSE IF  ( ifac .gt. 0 )  THEN
		CALL GDESCL  ( rec, iscale, ier )
C
C*	         If the word COLUMN has been found, this is the data.
C
	      ELSE IF  ( idata .gt. 0 )  THEN
C
C*		Backspace so that record can be read later.
C
		BACKSPACE  ( lun )
C
C*		Set flag to stop reading file.
C
		done = .true.
	    END IF
	END DO
C
C*	Check that identifier and size of grid have been found.
C
	IF  ( .not. sizfnd )  iret = -4
	IF  ( .not. idnfnd )  iret = -3
C*
	RETURN
	END
