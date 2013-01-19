	SUBROUTINE IP_PUTV  ( pname, parm, iret )
C************************************************************************
C* IP_PUTV								*
C*									*
C* This subroutine saves the value of a variable to the global list.	*
C*									*
C* IP_PUTV  ( PNAME, PARM, IRET )					*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		Variable name			*
C*	PARM		CHAR*		Variable value			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* K. Brill/GSFC	 4/90	Save value instead of variable name	*
C* K. Tyle/GSC		 7/96	Renamed from NT_ULOC			*
C* S. Jacobs/NCEP	 4/97	Renamed from IP_ULOC			*
C* D.W.Plummer/NCEP	 6/97	Increased string length from 72 to 128	*
C* D.W.Plummer/NCEP	 8/00	Add pname & parm to list if not found	*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pname, parm
C*
	CHARACTER	var*8, value*128
C-----------------------------------------------------------------------
	iret = 0
C
C*	Move the variables into other parameters.
C
	CALL ST_LCUC  ( pname, var, ier )
	value = parm
C
C*	Find this variable in the list.
C
	DO   ii = 1, ncparm
	    IF  ( var .eq. cparmn (ii) )  THEN
		cparmv (ii) = value
		RETURN
	    END IF
	END DO
C
C*      If not found, add to the list.
C
        IF  ( ncparm .ge. MXIPPM )  THEN
            iret = -3
            CALL ER_WMSG  ( 'IP', iret, ' ', ier )
            RETURN
        ELSE
            ncparm = ncparm + 1
            cparmn ( ncparm ) = var
            cparmv ( ncparm ) = value
        END IF
C
C*
	RETURN
	END
