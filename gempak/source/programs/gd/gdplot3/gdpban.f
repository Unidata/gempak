	SUBROUTINE GDPBAN ( string, nbang, maxb, strout, nbangs, iret )
C************************************************************************
C* GDPBAN							*
C*									*
C* This subroutine takes parameter input in "!"-parsable string 	*
C* (ie., GDATTIM) where the "!"s are grouped by parentheses		*
C* (eg., GDATTIM = 961105/12(F12!F24) ; 961105/12(F36!F48) )		*
C* and returns the string modified to include only the nbang element	*
C* substituted into the parenthetical expression			*
C* (eg., for nbang=1, GDATTIM=961105/12F12 ; 961105/12F36 )		*
C*									*
C* GDPBAN  ( STRING, NBANG, MAXB, STROUT, NBANGS, IRET )		*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input parameter string		*
C*	NBANG		INTEGER		Bang element to substitute	*
C*	MAXB		INTEGER		Max number of bangs expected	*
C*									*
C* Output parameters:							*
C*	STROUT		CHAR*		Output string			*
C*	NBANGS		INTEGER		Number of bangs found		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid string		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	11/96						*
C* D.W.Plummer/NCEP	 2/97	Add check for invalid () parings	*
C************************************************************************
	CHARACTER	string*(*), strout*(*), substr*256
	CHARACTER	gdbang(20)*64
C
	iret = 0
	strout = string
	nbangs = 1
C
C*	ilr and irp are pointers to left and right parentheses
C
	ilp = INDEX ( strout, '(' )
C
	    DO WHILE ( ilp .ne. 0 )
C
	        irp = INDEX ( strout, ')' )
		IF ( irp .lt. ilp )  THEN
		    iret = -1
		    RETURN
		END IF
		IF ( irp .eq. ilp+1 )  THEN
		    strout = strout(:ilp-1) // strout(irp+1:)
		ELSE
	            substr = strout(ilp+1:irp-1)
		    CALL GDINST ( substr, '<', maxb, .true.,
     +		                  gdbang, nbangs, iret )
		    CALL ST_LSTR ( gdbang(nbang), lgdb, iret )
      	            strout = strout(:ilp-1) // gdbang(nbang)(:lgdb)
     +	                // strout(irp+1:)
 		END IF
	        ilp = INDEX ( strout, '(' )
C
	    END DO
C
	RETURN
	END
