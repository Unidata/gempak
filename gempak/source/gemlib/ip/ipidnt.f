	SUBROUTINE IP_IDNT  ( progrm, iret )
C************************************************************************
C* IP_IDNT								*
C*									*
C* This subroutine saves the name of the program being executed.	*
C* The parameters used in the program are read from the PDF file.	*
C* If $RESPOND is set, a dynamic tutor is entered.			*
C*									*
C* IP_IDNT  ( PROGRM, IRET )						*
C*									*
C* Input parameters:							*
C*	PROGRM		CHAR*		Program name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	  0 = normal return		*
C*					-10 = pdf cannot be opened	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* D. Moore/OU-GCN	 6/89	Added call to NT_STRP and NT_DYNM	*
C* M. desJardins/GSFC	 7/89	Modified for GEMPAK 5			*
C* M. desJardins/GSFC	 4/90	Fixed error messages			*
C* S. Schotz/GSC	 5/90	Remove call to NT_STR 			*
C* K. Tyle/GSC		 7/96	Renamed from NT_IDNT			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	progrm
C*
	LOGICAL		done
C------------------------------------------------------------------------
	iret   = 0
C
C*	Save program name.
C
	CALL ST_LCUC  ( progrm, cprogm, ier )
C
C*	Get the program parameter names.
C
	CALL IP_STRP  ( cprogm, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check for whether the user wants to exit program.
C
	CALL IP_DYNM ( done, iret )
	IF  ( done )  THEN
	    CALL IP_EXIT ( iret )
	    CALL SS_EXIT
	END IF
C*
	RETURN
	END
