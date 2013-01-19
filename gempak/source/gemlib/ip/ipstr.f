	SUBROUTINE IP_STR  ( pname, parm, iret )
C************************************************************************
C* IP_STR	                                    			*
C*									*
C* This subroutine returns the value of a string variable from the	*
C* interface.  If the parameter does not have a value, PARM is set to 	*
C* blanks.  If no more variables can be added to the global table, an	*
C* error message is written and IRET is set to -3.			*
C*									*
C* IP_STR  ( PNAME, PARM, IRET )					*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		Variable name			*
C*									*
C* Output parameters:                                                   *
C*	PARM 		CHAR*		Variable value			*
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*					 -1 = too many variables	*
C*					 -3 = too many parameters	*
C**	                                                                *
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* D. Moore/OU-GCN	 6/89	Eliminate call to NT_DFLT		*
C* M. desJardins/GSFC	 4/90	Error messages				*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* K. Tyle/GSC		 7/96	Renamed from NT_STR			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	parm, pname
C*
	CHARACTER	ppp*8
C-----------------------------------------------------------------------
	iret   = 0
C
C*	Change parameter name to upper case.
C
	CALL ST_LDSP  ( pname, ppp, np, ier )
	CALL ST_LCUC  ( ppp, ppp, ier )
C
C*	Check to see if this parameter has been read in before.
C
	ilist = 0
	DO  i = 1, ncparm
	    IF  ( cparmn (i) .eq. ppp )  ilist = i
	END DO
C
C*	If not found, add to the list.
C
	IF  ( ilist .eq. 0 )  THEN
	    IF  ( ncparm .ge. MXIPPM )  THEN
		iret = -3
		CALL ER_WMSG  ( 'IP', iret, ' ', ier )
		RETURN
	      ELSE
		ncparm = ncparm + 1
		ilist  = ncparm
		cparmn ( ncparm ) = ppp
		cparmv ( ncparm ) = ' '
	    END IF
	END IF
C
C*	Now, get value for parameter and set APFLG.
C
	parm = cparmv ( ilist )
C*
	RETURN
	END
