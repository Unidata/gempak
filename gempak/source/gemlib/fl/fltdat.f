	SUBROUTINE FL_TDAT ( lun, iret )
C************************************************************************
C* FL_TDAT								*
C* 									*
C* This subroutine advances past the header comment records of a table	*
C* file and positions the file for reading from the first data record.	*
C* The file should have been opened by a call to FL_TBOP. 		*
C* 									*
C* A GEMPAK table file is a text file that may have comment records	*
C* within the file.  A comment record is a record where the first	*
C* non-blank character is an exclamation point.  Only the first 80	*
C* characters of each record are checked. 				*
C* 									*
C* FL_TDAT  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C* 	LUN		INTEGER		Logical unit number 		*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C* 					   0 = Normal return		*
C*					 -10 = No table records found	*
C* 					  -4 = Cannot read file		*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* J. Whistler/SSAI	 5/91	Removed declaration of TAB and SPACE	*
C* D. Keiser/GSC	12/95	Changed comments			*
C* K. Tyle/GSC		 1/96	Changed iret to -102 			*
C* G. Krueger/EAI	 8/96	Changed iret value; Changed comments	*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
C
	LOGICAL 	datfnd
	CHARACTER*80 	filrec
C------------------------------------------------------------------------
C*	Read records until a data record found.
C
	datfnd = .false.
	DO WHILE ( .not. datfnd )
	    READ ( lun, 100, IOSTAT = iostat ) filrec
100	    FORMAT ( A )
C
	    IF ( iostat .gt. 0 ) THEN		
		iret = -4
		RETURN
	      ELSE IF ( iostat .lt. 0 ) THEN
		iret = -10
		RETURN
	      ELSE
C
C*		Eliminate leading blank and tab characters.
C
		CALL ST_LDSP  ( filrec, filrec, length, ier )
C
C*		Check if the first character is a "!".
C
		IF ((length .eq. 0) .or. (filrec (1:1) .eq. '!')) THEN
		    datfnd = .false.
		  ELSE
		    datfnd = .true.
		END IF
	    END IF
	END DO
C
C*	Now that first data record has been found, backspace so it 
C*	can be read.
C
	CALL FL_BKSP ( lun, iret )
C*
	RETURN
	END
