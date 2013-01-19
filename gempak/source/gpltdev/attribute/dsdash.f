	SUBROUTINE DSDASH  ( szdsh, size, iret )
C************************************************************************
C* DSDASH								*
C* 									*
C* This subroutine sets the line dashing scale.  If the parameter is	*
C* not positive, no change is made.   					*
C* 									*
C* DSDASH  ( SZDSH, SIZE, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C* 	SZDSH		REAL		Line dashing scale		*
C*                                                                    	*
C* Output parameters:							*
C*	SIZE		REAL		Line dashing scale		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = CSDASH
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szdsh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	CALL GGETR  ( size, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	ELSE
C
C*	    Save the ACTIVE line dashing scale in common block 
C*	    variable.
C
	    tszdsh = size
	END IF
C*
	RETURN
	END
