	SUBROUTINE FL_REWD ( lun, iret )
C************************************************************************
C* FL_REWD								*
C* 									*
C* This subroutine rewinds a sequential file.				*
C* 									*
C* FL_REWD  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = cannot rewind file	*
C**									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* S. Maxwell/GSC	12/96	Modified return code; removed FL_IRET	*
C************************************************************************
C------------------------------------------------------------------------
C*	Rewind the file, get return code.
C
	REWIND ( UNIT = lun, IOSTAT = iostat )
C
	IF  ( iostat .ne. 0 )  THEN
	    iret = -7
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
