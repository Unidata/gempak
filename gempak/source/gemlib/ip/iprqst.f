	SUBROUTINE IP_RQST  ( iret )
C************************************************************************
C* IP_RQST								*
C*									*
C* This subroutine lists the variables used in the program.		*
C*									*
C* IP_RQST  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* M. desJardins/NMC	 2/92	Eliminate blanks at end of write buffer	*
C* K. Tyle/GSC		 7/96	Renamed from NT_RQST			*
C* D.W.Plummer/NCEP	 6/97	Increased string length from 80 to 136	*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER	output*136
C-----------------------------------------------------------------------
	iret   = 0
	output = 'Parameters requested: '
	lent   = 22
C
C*	Add variables to output buffer.
C
	i = ihead
	DO WHILE  ( i .gt. 0 )
C
C*	    Don't add global parameters.
C
	    IF  ( cparmn (i) (1:1) .ne. '$' )  THEN
		CALL ST_LSTR  ( cparmn (i), lenc, ier )
C
C*		Check to see if parameter will fit in buffer.
C
		IF  ( ( lent + lenc + 1 ) .gt. 76 )  THEN
		    WRITE  ( 6, 1000 )  output ( 1:lent )
1000		    FORMAT ( 1X, A )
		    lent   = 0
		    output = ' '
		END IF
C
C*		Add parameter with a comma.  
C
		output (lent+1: ) = cparmn (i) (1:lenc) // ','
		lent = lent + lenc + 1
	    END IF
	    j = iplink (i)
	    i = j
	END DO
C
C*	Write last buffer.  Replace last comma with a period.
C
	output ( lent : lent ) = '.'
	WRITE  ( 6, 1000 )  output ( 1:lent )
C*
	RETURN
	END
