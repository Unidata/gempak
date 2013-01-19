	SUBROUTINE DC_BSRC  ( string, stlist, nstr, ipos, iret )
C************************************************************************
C* DC_BSRC								*
C*									*
C* This subroutine searches for a specified string within an array of	*
C* strings using the binary search method.  The array of strings must	*
C* be lexically sorted in ascending order.  The position of the string	*
C* within the array is returned in IPOS.  If the string is not found,	*
C* then IPOS is set to 0.						*
C*									*
C* DC_BSRC  ( STRING, STLIST, NSTR, IPOS, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*	STLIST (NSTR)	CHAR*		Array of strings		*
C*	NSTR		INTEGER		Number of strings in array	*
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of string in array 	*
C*				 	  0 = not found			*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP         8/96                                            *
C* D. Kidwell/NCEP	4/97 	Renamed from UT_BSRC, cleaned up code   *
C* D. Kidwell/NCEP     12/97 	Renamed from MA_BSRC                    *
C************************************************************************
	CHARACTER*(*)	string, stlist (*)
C*
	LOGICAL		again
C-----------------------------------------------------------------------
	ipos = 0
	iret = 0
C
	IF ( nstr .gt. 1 ) THEN
C
	    istart = 1
	    iend   = nstr
	    again  = .true.
C
	    DO WHILE  ( again )
	        imid = ( istart + iend ) / 2
	        IF  ( string .eq. stlist ( imid ) )  THEN
		    again = .false.
		    ipos = imid
	          ELSE IF  ( ( istart + 1 ) .eq. iend )  THEN
C
C*		    istart and iend are consecutive integers.
C
	 	    again = .false.
		    IF  ( string .eq. stlist ( istart ) )  THEN
		        ipos = istart
	 	      ELSE IF  ( string .eq. stlist ( iend ) )  THEN
		        ipos = iend
		    END IF
	          ELSE IF  ( LLT ( string, stlist ( imid ) ) )  THEN
C
C*		    Have stlist ( istart ) <= string < stlist ( imid ).
C
		    iend = imid
	          ELSE
C
C*		    Have stlist ( imid )  < string <=  stlist ( iend ).
C
		    istart = imid
	        END IF
	    END DO
	  ELSE IF ( nstr .eq. 1 ) THEN
C
C*	    Handle the special case where nstr = 1.
C
	    IF  ( string .eq. stlist (1) ) ipos = 1
	END IF
C*
	RETURN
	END
