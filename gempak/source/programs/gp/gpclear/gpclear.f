	PROGRAM GPCLEAR
C************************************************************************
C* PROGRAM GPCLEAR							*
C*									*
C* This program clears the current graphics device.			*
C*									*
C**									*
C* Log:									*
C* J. Woytek/GSFC	11/82	Original source				*
C* M. Goodman/RDS	 8/84	Updated with new GEMPLT routines	*
C* I. Graffman/RDS	 9/85	Added device				*
C* S. Schotz/GSC	 8/90	Added call to IP_IDNT			*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* K. Tyle/GSC		 7/96	Eliminated call to IP_USTR		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	LOGICAL		respnd
	CHARACTER	device*(LLMXLN)
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPCLEAR', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 0, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPCLEAR', -3, ' ', ier )
	    CALL IP_EXIT  ( ier )
	    CALL SS_EXIT
	END IF
C*
	CALL IP_IDNT ( 'GPCLEAR', ier )
C
C*	Get device variable.
C
	CALL IP_STR  ( 'DEVICE', device, ier )
	IF  ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GPCLEAR', ier, ' ', iret )
        END IF
C
C*	Set the device.
C
	IF  ( ier .eq. 0 )  THEN

	    CALL GG_SDEV  ( device, ier )
C
C*	    Clear screen.
C
	    IF  ( ier .eq. 0 )  THEN
		CALL GCLEAR  ( iret )
	    END IF
	END IF
C
C*	Exit from program.
C
	CALL IP_EXIT  ( iret )
	CALL GENDP  ( 0, ier )
	END
