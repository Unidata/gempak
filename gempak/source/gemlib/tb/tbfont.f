	SUBROUTINE TB_FONT ( fname, fsize, iret )
C************************************************************************
C* TB_FONT								*
C*									*
C* This subroutine reads a table of text size names and values and 	*
C* returns the value of the name matching the input string.		*
C*									*
C* TB_FONT ( FNAME, FSIZE, IRET )					*
C*									*
C* Input parameters:							*
C*	FNAME		CHAR*		Text size name			*
C*									*
C* Output parameters:							*
C*	FSIZE		REAL		Text size value			*
C*      IRET            INTEGER         Return code                     *
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/98						*
C* C. Lin/EAI	 	 8/98    modified to read X fontsz too  	*
C************************************************************************
        CHARACTER*(*)   fname
C*
	CHARACTER	fnmuc*12, font*12
	LOGICAL		found
C------------------------------------------------------------------------
        iret = 0
C
C*      Convert alias to upper case.
C
	CALL ST_LCUC ( fname, fnmuc, ier )
C
C*      Open table.
C
	CALL FL_TBOP ( 'fontsz.tbl', 'config', lun, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG  ( 'FL', ier, 'fontsz.tbl', ierr )
	    RETURN
	END IF
C
	found  = .false.
	iostat = 0
C
C*      Match alias with its parameter list.
C
	DO WHILE  ( ( iostat .eq. 0 ) .and. ( .not. found ) )
	    READ  ( lun, 2, IOSTAT = iostat )  font, size, itmp
2	    FORMAT ( A12, 1X, F7.3, 10X, I2 )
	    IF  ( ( iostat .eq. 0 ) .and.
     +		  ( font(1:1) .ne. '!' ) ) THEN
		IF  ( font(1:1) .eq. fnmuc(1:1) )  THEN
		    found = .true.
		    fsize = size
		END IF
	    END IF
	END DO
C
C*      Close table.
C
	CALL FL_CLOS ( lun, ier )
C
C*	If the entry was not found, return a warning.
C
	IF  ( .not. found )  THEN
	    iret  = 4
	    fsize = 0.0
	END IF
C*
	RETURN
	END
