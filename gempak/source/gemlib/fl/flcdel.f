	SUBROUTINE FL_CDEL ( lun, iret )
C************************************************************************
C* FL_CDEL								*
C* 									*
C* This subroutine closes and deletes a file that was opened by any	*
C* FL subroutine and frees the assigned logical unit number.  Note 	*
C* that this uses a non-standard FORTRAN option so that the file may 	*
C* not be deleted on UNIX systems.					*
C* 									*
C* FL_CDEL  ( LUN, IRET )						*
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
C* G. Chatters/RDS	 3/84	  					*
C* I. Graffman/RDS 	 7/84	Copied and modified			*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* M. desJardins/GSFC	12/90	Remove DELETE for UNIX			*
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
