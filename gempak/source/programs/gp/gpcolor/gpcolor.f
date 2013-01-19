	PROGRAM GPCOLOR
C************************************************************************
C* GPCOLOR								*
C*									*
C* This program changes the color components of a graphics plane.	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 9/85						*
C* M. Goodman/RDS	11/85	Added dynamic tutor			*
C* M. desJardins	 1/86	Added GFLUSH and moved GPCTAE call	*
C* I. Graffman/RDS	 2/88	Moved IP_USTR - do if successful 	*
C* I. Graffman/RDS	 5/88	Changed GFLUSH to GEPLOT		*
C* G. Huffmann/GSC	10/88	Error messages				*
C* S. Schotz/GSC	 8/90	Added IP_IDNT, and clean up		*
C* S. Schotz/GSC         10/90  Fixed for lower case inputs     	*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* K. Brill/NMC		01/92	Changes for using IN_COLR to do it all	*
C* L. Williams/EAI       3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* S. Jacobs/NMC         6/94   COLORS*24 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to GPCUPD			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	PARAMETER	( NEXP = 32 )
C*
	CHARACTER 	colors*(LLMXLN), device*(LLMXLN)
C*
	INTEGER		icolr ( NEXP )
	LOGICAL 	respnd, done
C------------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPCOLOR', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 0, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPCOLOR', -3, ' ', ier )
	    CALL IP_EXIT  ( ier )
	    CALL SS_EXIT
	END IF
C*
	CALL IP_IDNT ( 'GPCOLOR', ier )
C
C*	Loop until user quits.
C
	done = .false.
	DO WHILE  ( .not. done ) 
C
C*          Get input variables.
C
	    CALL GPCINP  ( device, colors, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'GPCOLOR', -2, ' ', ier )
		CALL IP_EXIT  ( ier )
		CALL SS_EXIT
	    END IF
C
C*	    Set the requested device.
C
	    CALL GG_SDEV  ( device, iret )
C
C*	    Check that device was set; if so, set colors. 
C
	    IF  ( iret .eq. 0 )  THEN
C
C*		Get a set of color numbers.
C
		CALL IN_COLR ( colors, NEXP, icolr, iret )
		CALL GEPLOT  ( ier )
	    END IF
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM  ( done, iret )
	END DO
C
C*	Exit.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
	END
