	SUBROUTINE FL_CLOS ( lun, iret )
C************************************************************************
C* FL_CLOS								*
C* 									*
C* This subroutine closes a file that was opened by a FL subroutine 	*
C* and frees the assigned logical unit number.				*
C* 									*
C* FL_CLOS  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = cannot close file		*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* M. desJardins/GSFC	 3/87	Rewrote					*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
C------------------------------------------------------------------------
C*	Close the file, get return code.
C
	CLOSE ( UNIT = lun, IOSTAT = iostat )
C*
	IF ( iostat .ne. 0 ) THEN
	    iret = -2
	  ELSE
	    iret = 0
	END IF
C
C*	Free the logical unit number.
C
	CALL FL_FLUN ( lun, ier )
C*
	RETURN
	END
