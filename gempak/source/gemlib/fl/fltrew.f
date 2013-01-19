	SUBROUTINE FL_TREW ( lun, iret )
C************************************************************************
C* FL_TREW								*
C* 									*
C* This subroutine rewinds a table file that was opened by FL_TBOP.	*
C* The file is positioned to read the first data record in the file.	*
C* 									*
C* FL_TREW  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					<>0 = GEMPAK file error 	*
C**									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86		Changed comments		*
C* D. Keiser/GSC	12/95		Changed comments		*
C************************************************************************
C------------------------------------------------------------------------
C*	Rewind file.
C
	CALL FL_REWD ( lun, iret )
C
C*	If rewind successful, advance to first data record.
C
	IF  ( iret .eq. 0 )  THEN
	    CALL FL_TDAT  ( lun, iret )
	END IF
C*
	RETURN
	END
