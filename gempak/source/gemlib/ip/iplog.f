	SUBROUTINE IP_LOG  ( pname, logprm, iret )
C************************************************************************
C* IP_LOG	                                    			*
C*									*
C* This subroutine returns the value of a logical variable.  A		*
C* YES or NO entered in the interface is converted to TRUE or FALSE.	*
C* If the first letter of the input is not "Y", the value is set	*
C* to FALSE.								*
C*									*
C* IP_LOG  ( PNAME, LOGPRM, IRET )					*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		Name of variable		*
C*									*
C* Output parameters:                                                   *
C*	LOGPRM 		LOGICAL		Variable value			*
C*	IRET		INTEGER 	Return code			*
C*				   	   0 = normal return		*
C*			          	  -2 = parameter not received	*
C**	                                                                *
C* Log:									*
C* M. desJardins/NMC	 6/94	Copied from IP_STR			*
C* K. Tyle/GSC		 7/96	Renamed from NT_LOG			*
C************************************************************************
	CHARACTER*(*) 	pname
	LOGICAL		logprm 
C*
	CHARACTER	parm*12
C-----------------------------------------------------------------------
C*	Get string corresponding to variable.
C
	CALL IP_STR  ( pname, parm, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Translate to logical.
C
	CALL ST_LCUC  ( parm, parm, ier )
	IF  ( parm (1:1) .eq. 'Y')  THEN
	    logprm = .true.
	  ELSE
	    logprm = .false.
	END IF
C*
	RETURN
	END
