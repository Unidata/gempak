	LOGICAL FUNCTION RS_NMER ( lets, numb )
C************************************************************************
C* RS_NMER								*
C*									*
C* This function determines whether the input letters are ASCII digits.	*
C*									*
C* LOGICAL RS_NMER  ( LETS, NUMB )					*
C*									*
C* Input parameters:							*
C*	LETS		CHAR*		String				*
C*	NUMB		INTEGER		Number of letters		*
C*									*
C* Output parameters:							*
C*	RS_NMER		LOGICAL		Return value			*
C*					= .TRUE. if letters are digits, *
C*						between '0' and '9'	*
C*					= .FALSE. if any letter is not 	*
C*						a digit			*
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/TAMU	 2/92	Gempacized				*
C************************************************************************
	CHARACTER*(*)	lets
	INTEGER		numb	
C------------------------------------------------------------------------
	RS_NMER = .TRUE.
	DO  i = 1, numb
	    IF  ( ( lets(i:i) .lt. '0' ) .or. 
     +		  ( lets(i:i) .gt. '9' ) ) THEN
	        RS_NMER = .FALSE.
	        RETURN
	    ENDIF
	END DO
	RETURN
	END				

