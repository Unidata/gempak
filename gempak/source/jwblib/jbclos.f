	SUBROUTINE JB_CLOS  ( ifn, iret )
C************************************************************************
C* JB_CLOS								*
C*									* 
C* This subroutine closes a Jack Woollen BUFR data file.		*
C*									*
C* JB_CLOS  ( IFN, IRET )						*
C*									*
C* Input parameters:							*
C*	IFN		INTEGER		File unit number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C**									*
C* Log:									*
C* K. Brill/EMC		10/96						*
C* K. Brill/EMC		12/96	Remove current file feature		*
C************************************************************************
C*-----------------------------------------------------------------------
	iret = 0
C*
	CALL CLOSBF ( ifn )
C*
	RETURN
	END
