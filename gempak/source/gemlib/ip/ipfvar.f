	SUBROUTINE IP_FVAR  ( var, ifound, iret )
C************************************************************************
C* IP_FVAR								*
C*									*
C* This subroutine checks the variable name to see if it is an		*
C* abbreviation for a variable in the program variable list.  If	*
C* the name is not unique, an error message is written and an		*
C* error is returned.							*
C*									*
C* IP_FVAR  ( VAR, IFOUND, IRET )					*
C*									*
C* Input parameters:							*
C*	VAR		CHAR*		Variable name			*
C*									*
C* Output parameters:							*
C*	IFOUND		INTEGER		Location in variable list	*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					 -8 = ambiguous variable	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* M. desJardins/NMC	 2/92	Improve search for program variables	*
C* K. Tyle/GSC		 7/96	Renamed from NT_FVAR			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	var
C*
	CHARACTER	vvv*8
C-----------------------------------------------------------------------
	iret   = 0
	ifound = 0
C
C*	Make sure there are no blanks in the variable name.
C
	CALL ST_RMBL  ( var, vvv, lenvar, ier )
	CALL ST_LCUC  ( vvv, vvv, ier )
C
C*	Find this variable in the list of program parameters.
C
	nfound = 0
	i = ihead
	DO WHILE  ( i .gt. 0 )
	    IF  ( vvv (1:lenvar) .eq. cparmn (i) (1:lenvar) )  THEN
		nfound = nfound + 1
		ifound = i
	    END IF
	    i = iplink (i)
	END DO
C
C*	Check that only one variable was found.
C
	IF  ( nfound .gt. 1 )  THEN
	    iret = -8
	    CALL ER_WMSG  ( 'IP', iret, vvv, ierr )
	    ifound = 0
	    RETURN
	END IF
C*
	RETURN
	END
