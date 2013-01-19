	SUBROUTINE FL_BKSP ( lun, iret )
C************************************************************************
C* FL_BKSP								*
C* 									*
C* This subroutine backspaces a sequential file.			*
C* 									*
C* FL_BKSP  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = cannot backspace in file	*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
C------------------------------------------------------------------------
C*	Backspace the file, get return code.
C
	BACKSPACE ( UNIT = lun, IOSTAT = iostat )
C*
	IF ( iostat .ne. 0 ) THEN
	    iret = -8
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
