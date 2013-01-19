	SUBROUTINE IP_INIT  ( respnd, iret )
C************************************************************************
C* IP_INIT								*
C*									*
C* This subroutine initializes the GEMPAK user interface.		*
C* The file GEMGLB.NTS is read to obtain global variable values.  	*
C* An error opening the file with write access will prevent global	*	
C* values from being saved.						*
C*									*
C* The variable RESPND returns the logical value of the global variable	*
C* $RESPOND.  Whenever this value is FALSE, the program should not	*
C* expect input from the user.  If the user is executing the program	*
C* in batch mode, the respond flag will also be set to false.		*
C*									*
C* IP_INIT  ( RESPND, IRET )						*	
C*									*
C* Output parameters:							*
C*	RESPND		LOGICAL		Respond flag			*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C*					 -1 = too many variables	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* D. Moore/OU-GCN	 6/89	Rewrote                                 *
C* M. desJardins/GSFC	 4/90	Added file name to common		*
C* K. Tyle/GSC		 7/96	Renamed from NT_INIT			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret   = 0
C
C*	Initialize variables in common.
C
	CALL IN_BDTA ( ier )
C
C*	Initialize common variables.
C
	ncparm = 0                                   
	cprogm = ' '
C
C*	Read the initialization file and initialize the variables.
C
	CALL IP_DFLT  ( iret )
 
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'IP', iret, IPFIL, ier )
	END IF
C
C* 	Get value of $RESPOND.
C
	CALL IP_RESP ( respnd, ierr )
C*
	RETURN
	END
