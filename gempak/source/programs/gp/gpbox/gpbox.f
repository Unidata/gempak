	PROGRAM GPBOX
C************************************************************************
C* GPBOX								*
C*									*
C* This program draws a box around a region on the display device.	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 1/85						*
C* I. Graffman/RDS	 8/86	Common interface and errors		*
C* G. Huffman/GSC	10/88	GEMPAK4 version				*
C* M. desJardins/GSFC	 9/90	GEMPAK5					*
C* S. Jacobs/NMC	 6/94	DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to GPBUPD			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
 	CHARACTER 	device*(LLMXLN), region*(LLMXLN), line*(LLMXLN)
C*
	LOGICAL 	respnd, scflag
C-----------------------------------------------------------------------
C* 	Set up user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPBOX', -1, ' ', ier )
	    CALL SS_EXIT
	  ELSE
	    CALL IP_IDNT  ( 'GPBOX', ier )
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 0, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPBOX', -3, ' ', ier )
	    CALL IP_EXIT  ( ier )
	    CALL SS_EXIT
	END IF
C
C*	Get line attributes
C
	CALL GPBINP  ( line, region, device, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*	    Set device.
C
	    CALL GG_SDEV  ( device, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*	        Draw box.
C
		CALL IN_LINE  ( line, 0., 1, icolor, itype, iwidth,
     +				ilabel, smth, fltr, scflag, iret )
	        CALL GG_BOX  ( region (1:1), icolor, itype, iwidth,
     +	                       iret )
	    END IF
	END IF
C
C*	Final error message and clean up.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GPBOX', iperr, ' ', ier )
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C
	END
