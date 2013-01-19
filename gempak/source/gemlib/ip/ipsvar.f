	SUBROUTINE IP_SVAR  ( input, iret )
C************************************************************************
C* IP_SVAR								*
C*									*
C* This subroutine sets the value of a variable in a dynamic tutor.	*
C* It assumes that input is in the form VAR=VALUE.  The variable 	*
C* may be a unique abbreviation of a current program variable.  If	*
C* the variable is any other global variable, the full name must 	*
C* be input.								*
C*									*
C* IP_SVAR  ( INPUT, IRET )						*
C*									*
C* Input parameters:							*
C*	INPUT		CHAR*		Input string			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* D. Moore/OU-GCN	 6/89	Translate DEVICE as a symbol		*
C* M. desJardins/GSFC	 4/90	Comment out DEVICE-problem on UNIX	*
C* J. Whistler/SSAI	 5/91	Allow transfer of one variable to 	*
C*				another by typing "&variable"		*
C* K. Tyle/GSC		 7/96	Renamed from NT_SVAR			*
C* D.W.Plummer/NCEP	 6/97	Increased string length from 72 to 128	*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	input
C*
	CHARACTER	vvv*16, var*8, value*128
C-----------------------------------------------------------------------
	iret = 0
C
C*	Find the = which separates the two parts.
C
	iequal = INDEX ( input, '=' )
	IF  ( iequal .le. 1 )  RETURN
C
C*	Make sure there are no blanks in the variable name.
C
	vvv = input ( : iequal - 1 )
	CALL ST_RMBL  ( vvv, var, lenvar, ier )
	CALL ST_LCUC  ( var, var, ier )
C
C*	Remove only leading blanks in value.
C
	value = input ( iequal + 1 : )
	CALL ST_LDSP  ( value, value, lenval, ier )
C
C*	Find double quotes and remove.
C
	IF  ( ( value (1:1) .eq. '"' ) .and. 
     +	      ( value (lenval:lenval) .eq. '"' ) )  
     +					value = value ( 2 : lenval-1 )
C
C*	Find this variable in the list of program parameters.
C
	CALL IP_FVAR  ( var, ifound, ier )
	IF  ( ier .ne. 0 )  RETURN
C
C*	If name was not found, check for parameter name in full list.
C
	IF  ( ifound .eq. 0 )  THEN
	    DO  i = 1, ncparm
		IF  ( var .eq. cparmn (i) )  ifound = i
	    END DO
	END IF
C
C*	Get value of parameter.
C
	IF  ( ifound .ne. 0 )  THEN
C
C*	    Check to see if the value of parameter points to another
C*	    parameter value by checking for an "&" at the beginning
C*	    of value.
C
	    ivalpm = INDEX ( value, '&' )
C*
	    ifnd1 = 0 
	    IF ( ivalpm .eq. 1 )  THEN
		value = value (2: )
C
C*		Check to see if this variable is in the list of program
C*		parameters.
C
		CALL IP_FVAR  ( value, ifnd1, ier )
		IF  ( ier .ne. 0 )  RETURN
C
C*		If name was not found, check for parameter name in full
C*		list.
C
		IF  ( ifnd1 .eq. 0 )  THEN
		    DO  i = 1, ncparm
			IF  ( value .eq. cparmn (i) )  ifnd1 = i
		    END DO
		END IF
	    END IF
C
C*		If name was found, set value equal to value of the 
C*		parameter.
C
	    IF ( ifnd1 .ne. 0 )  THEN
		value = cparmv ( ifnd1 )
		cparmv ( ifound ) = value
	      ELSE IF ( ( ifnd1 .eq. 0 ) .and. ( ivalpm .eq. 1 ) )  THEN
		ier = -7
		CALL ER_WMSG  ( 'IP', ier, value, ierr )
	      ELSE
		cparmv ( ifound ) = value
	    END IF
	  ELSE
	    ier = -7
	    CALL ER_WMSG  ( 'IP', ier, var, ierr )
	END IF
C*
	RETURN
	END
