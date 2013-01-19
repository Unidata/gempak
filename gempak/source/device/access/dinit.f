	SUBROUTINE DINIT
C************************************************************************
C* DINIT								*
C*									*
C* This subroutine initializes device independent variables for the	*
C* device driver							*
C*									*
C* DINIT								*
C*									*
C**									*
C* Log:									*
C* S. Schotz/GSC	 2/90						*
C* M. desJardins/GSFC	 9/90	Add dummy call to DSCNAM		*
C* S. Jacobs/NCEP	 3/97	Changed line pattern number 9		*
C* S. Jacobs/NCEP	 6/98	Added init for special line patterns	*
C* A. Hardy/GSC		10/98	Added init for combination symbols      *
C* S. Jacobs/NCEP	11/98	Fixed typo: SYMNUM -> NSYNUM		*
C* S. Jacobs/NCEP	10/07	Added call to CTB_HFREAD		*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
C*
	INTEGER		llpat (8,10)
	CHARACTER	buffer*80, tarr(2)*8, sarr(5)*12
C
C*	Line patterns for all devices are initialized in the following
C*	data statement.  Line patterns are number of units on, number
C* 	off, number on etc.  Negative numbers indicate dots are to be
C*      drawn.  The first two patterns are all dot and a solid line, 
C*	respectively.
C
	DATA llpat / -3, -3, 0, 0, 0, 0, 0, 0,
     +		      1000000, 0, 0, 0, 0, 0, 0, 0,
     +                      2, 3, 0, 0, 0, 0, 0, 0,
     +			    4, 4, 0, 0, 0, 0, 0, 0,
     +			    6, 3, 2, 3, 0, 0, 0, 0,
     +			    5, 2, 0, 0, 0, 0, 0, 0,
     +			    8, 3, 2, 3, 2, 3, 2, 3,
     +			    8, -4, 0, 0, 0, 0, 0, 0,
     +			    6, -3, -3, -3, 0, 0, 0, 0,
     +			    15, -5, -5, 0, 0, 0, 0, 0 /
C------------------------------------------------------------------------
C* 	Save line patterns into common area
C
	DO i = 1, 8
	   DO j = 1, 10
	      lpat (i,j) = llpat (i,j)
	   END DO
	END DO
C
C*	Read the special line patterns from the table into
C*	the common area.
C
C*	Initialize the arrays.
C
	DO j = 1, LLNPAT
	   isptyp (j) = 0
	   DO i = 1, LLNSEG
	      isppat (i,j) = 0
	      ispatt (i,j) = 0
	      ispclr (i,j) = 0
	   END DO
	END DO
C
C*	Open the pattern table.
C
	CALL FL_TBOP ( 'splpat.tbl', 'draw', lunt, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ierr, 'splpat.tbl', ier )
	  ELSE
	    ios = 0
	    knt = 1
	    DO WHILE ( ios .eq. 0 )
2		FORMAT ( A )
C
C*		Read the table looking for a line starting with "TYPE".
C
		READ ( lunt, 2, IOSTAT = ios ) buffer
		IF  ( ( ios .eq. 0 ) .and.
     +		      ( buffer (1:4) .eq. 'TYPE' ) )  THEN
C
C*		    Get the type number and the attributes for 
C*		    each segment for each pattern.
C
		    CALL ST_CLST ( buffer, ' ', ' ', 2, tarr,
     +				   num, ier )
		    CALL ST_NUMB ( tarr(2), isptyp(knt), ier )
C
		    READ ( lunt, 2 ) buffer
		    CALL ST_ILST ( buffer, ' ', 0, LLNSEG,
     +				   isppat(1,knt), num, ier )
C
		    READ ( lunt, 2 ) buffer
		    CALL ST_ILST ( buffer, ' ', 0, LLNSEG,
     +				   ispatt(1,knt), num, ier )
C
		    READ ( lunt, 2 ) buffer
		    CALL ST_ILST ( buffer, ' ', 0, LLNSEG,
     +				   ispclr(1,knt), num, ier )
C
		    knt = knt + 1
		END IF
	    END DO
C
C*	    Close the table.
C
	    CALL FL_CLOS ( lunt, ier )
	END IF
C
C*	Read the combination symbol table into the common area.
C
C*	Initialize the arrays.
C
	DO j = 1, NSYNUM
            isycod (j) = 0
            DO i = 1, 2
	        ccsym (i,j) = ' '
	        rcmbsy (i,j) = 0.
            END DO
	END DO
C
C*	Open the combination symbol table.
C
	CALL FL_TBOP ( 'combo.tbl', 'draw', lunt, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ierr, 'combo.tbl', ier )
	  ELSE
	    ios = 0
	    knt = 1
	    DO WHILE ( ios .eq. 0 )
                READ ( lunt, 2, IOSTAT = ios ) buffer
                IF   ( ios .eq. 0 ) THEN
C
C*		    Get the combination number, type and symbol numbers
C*		    for each combination symbol.
C
		    CALL ST_CLST ( buffer, ' ', ' ', 5, sarr,
     +				   num, ier )
		    CALL ST_NUMB(sarr(1),  isycod(knt), ier )
		    ccsym (1,knt) = sarr(2)  
		    CALL ST_CRNM ( sarr(3), rcmbsy(1,knt), ier )
		    ccsym (2,knt) = sarr(4)  
		    CALL ST_CRNM ( sarr(5), rcmbsy(2,knt), ier )
                    knt = knt + 1
		END IF
	    END DO
C
C*	    Close the table.
C
	    CALL FL_CLOS ( lunt, ier )
	END IF
C
C*	Read the Hershey Font tables and save the fonts for later use.
C
	CALL CTB_HFREAD ( ier )
C
C*	S. Chiswell added check for ier==0
C
	IF ( ier .ne. 0 ) CALL ER_WMSG ( 'CTB', ier, ' ', ier2 )
C
C*	Add call to DSCNAM so that this subroutine will be found
C*	in the DEVICE library when the software is linked.
C
	CALL DSCNAM  ( -1, ' ', ier )
C
C*	Initialize filflg ( the fill hardware flag to indicate
C*	there sw filling only).  Also, set evtflg.
C
	filflg = .false.
	evtflg = .false.
C*
	RETURN
	END
