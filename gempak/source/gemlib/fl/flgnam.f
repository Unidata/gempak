	SUBROUTINE FL_GNAM  ( lun, filnam, iret )
C************************************************************************
C* FL_GNAM								*
C* 									*
C* This subroutine returns the file name associated with a logical	*
C* unit number.								*
C* 									*
C* FL_GNAM  ( LUN, FILNAM, IRET )					*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C* 									*
C* Output parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = cannot read file		*
C*					 -9 = unit not open		*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* M. desJardins/NMC	 8/94	Documentation				*
C* K. Tyle/GSC		12/95	Changed iret to -103			*
C* G. Krueger/EAI	 8/96	Changed iret value			*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
	CHARACTER*(*)	filnam
C*
	LOGICAL		opened
C------------------------------------------------------------------------
C*	Do an INQUIRE and return the name found if any.
C
	INQUIRE ( UNIT = lun, NAME = filnam, OPENED = opened,
     +		  IOSTAT = iostat )
C
C*	Determine return code.
C
	IF ( iostat .ne. 0 ) THEN
	    filnam = ' '
	    iret = -4
	  ELSE IF ( .not. opened ) THEN
	    filnam = ' '
	    iret = -9
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
