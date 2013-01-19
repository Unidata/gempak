	SUBROUTINE RU_GRPT  ( bultin, lenb, ibpnt, report, lenr, iret )
C************************************************************************
C* RU_GRPT								*
C*									*
C* This subroutine finds the next report in an upper-air bulletin.	*
C* Upon entry, IBPNT points to the character to begin the search.	*
C* The report returned will begin with TT, PP, UU, or XX and will 	*
C* terminate with =, the start of another report, or the end of 	*
C* the bulletin.							*
C*									*
C* RU_GRPT  ( BULTIN, LENB, IBPNT, REPORT, LENR, IRET )			*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Upper-air bulletin		*
C*	LENB		INTEGER		Length of bulletin		*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin		*
C*									*
C* Output parameters:							*
C*	REPORT		CHAR*		Report				*
C*	LENR		INTEGER		Length of report		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no more reports		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* B. Doty/RDS		 9/87		Use ST_NXTS			*
C* M. desJardins/GSFC	12/87						*
C* S. Jacobs/EAI (DOB)	 3/93		Changed comp of end of report	*
C* D. Kidwell/NCEP	 3/01		Added check for report 'XX'     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, report
C*
	LOGICAL		done
	CHARACTER	cc*2
	CHARACTER	stlist (5)*2, stlstx (5)*4
	INTEGER		lenstl (5), lenstx (5)
C*
	DATA		stlist  / 'TT', 'PP', 'UU', 'XX', '=' /
	DATA		lenstl  /    2,    2,    2,    2,   1 /
	DATA		stlstx  / 'XXAA', 'XXBB', 'XXCC', 'XXDD', '=' /
	DATA		lenstx  /      4,      4,      4,      4,   1 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for end of bulletin.
C
	IF  ( ibpnt .gt. lenb )  THEN
	    report = ' '
	    iret   = -1
	    RETURN
	END IF
C
C*	Look for the start of the next bulletin.
C
	done   = .false.
	ipoint = ibpnt
	DO WHILE  ( .not. done )
C
C*	    Look for the first four list items: TT, PP, UU, XX.
C
	    CALL ST_NXTS ( bultin, ipoint, lenb, stlist, lenstl, 4, 
     +			   ipos,   istrg, ierr ) 
C
C*	    If not found, return.
C
	    IF  ( ipos .eq. 0 )  THEN
		report = ' '
		lenr   = 0
		ibpnt  = lenb + 1
		iret   = -1
		RETURN
C
C*		Otherwise, check that next characters are correct.
C
	      ELSE
		cc = bultin ( ipos + 2 : ipos + 3 )
		IF  ( ( cc .eq. 'AA' ) .or. ( cc .eq. 'BB' ) .or.
     +		      ( cc .eq. 'CC' ) .or. ( cc .eq. 'DD' ) )  THEN
		    done = .true.
		END IF
		ipoint = ipos + 1
	    END IF
	END DO
C
C*	Look for the end of this report.  This time use all
C*	five list items to look for the = sign also.
C
	ipnt2 = ipos + 2
	IF ( istrg .ne. 4 ) THEN
	    CALL ST_NXTS  ( bultin, ipnt2, lenb, stlist, lenstl, 5, 
     +			    ipos2,  istrg, ierr ) 
	  ELSE
C
C*	    Since the 'XX' (dropsonde) report can include text, the
C*	    end of report check for it must be more restrictive.
C
	    CALL ST_NXTS  ( bultin, ipnt2, lenb, stlstx, lenstx, 5, 
     +			    ipos2,  istrg, ierr ) 
	END IF
C
C*	Get the end of the report.
C
	IF  ( ipos2 .eq. 0 )  THEN
	    iend = lenb
	  ELSE
	    iend = ipos2 - 1
	END IF
C
C*	Return the report.
C
	report = bultin ( ipos : iend )
	lenr   = iend - ipos + 1
	ibpnt  = iend + 1
	IF  ( istrg .eq. 5 )  ibpnt = ibpnt + 1
C*
	RETURN
	END
