	PROGRAM GPCLOSE
C************************************************************************
C* GPCLOSE								*
C*									*
C* This program closes the intermediate plot file or the selected	*
C* window. It is assumed that a device is already running.		*
C*									*
C**									*
C* Log:									*
C* J. M. Vilardo/RDS	 2/85						*
C* I. Graffman/RDS	 2/88	Error messages				*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* S. Jacobs/NMC	 8/94	Added DEVICE input variable to close	*
C*				  a specified window or file		*
C* S. Jacobs/NMC	 1/95	Added call to GEPLOT to set cur window	*
C* S. Jacobs/NCEP	 5/96	Removed call to GG_SDEV.		*
C* S. Jacobs/NCEP	 5/96	Added check for blank window name	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER	device*(LLMXLN)
C*
	LOGICAL 	respnd, done
C*
	CHARACTER	carr (4)*72
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPCLOSE', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 0, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPCLOSE', -3, ' ', ier )
	    CALL IP_EXIT  ( ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT ( 'GPCLOSE', ier )
C
	DO  WHILE ( .not. done )
C
C*	    The user input is a device name with a window/file name.
C*	    This program will close the specified window/file.
C
	    CALL GPOINP ( device, iperr )
	    IF  ( iperr .eq. 0 )  THEN
C
C*		Parse device string into components.
C
		CALL ST_CLST ( device, '|', ' ', 4, carr, num, ier ) 
C
C*		Use the second element to close the requested window.
C*		Unless the window name is blank, then write a warning.
C
		IF  ( carr (2) .eq. ' ' )  THEN
		    CALL ER_WMSG  ( 'GPCLOSE', 1, ' ', ier )
		  ELSE
		    CALL GSLWIN  ( carr (2), iret )
		    IF  ( ( iret .ne. 0 ) .and.
     +			  ( iret .ne. -39 ) )  THEN
			CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
		      ELSE
			CALL GCLOSP  ( iret )
			IF  ( iret .ne. 0 )
     +		    	    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
		    END IF
C
		END IF
	    END IF
C
	    CALL GEPLOT ( ier )
	    CALL IP_DYNM ( done, iret )
	END DO
C
C*	Exit after closing file.
C
	IF  ( iperr .ne. 0 )
     +	    CALL ER_WMSG  ( 'GPCLOSE', iperr, ' ', ier )
	CALL GENDP  ( 0, ier )
	CALL IP_EXIT  ( ier )
	END
