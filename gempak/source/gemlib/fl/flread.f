	SUBROUTINE FL_READ ( lun, irec, lenw, iarray, iret )
C************************************************************************
C* FL_READ								*
C* 									*
C* This subroutine reads a record from a direct access file.  On VMS    *
C* systems, if the record is locked by another user, 30 tries to open   *
C* the file will be attempted at 1-second intervals.                    *
C* 									*
C* FL_READ  ( LUN, IREC, LENW, IARRAY, IRET )				*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*	IREC		INTEGER		Record number			*
C*	LENW		INTEGER		Record length in words		*
C* 									*
C* Output parameters:							*
C*	IARRAY (LENW)	INTEGER		Data record			*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*				  	 -4 = cannot read file		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/86						*
C* M. desJardins/GSFC	 3/87	Changed from bytes to words		*
C* M. desJardins/GSFC	 2/90	UNIX					*
C* S. Maxwell/GSC	12/96	Modified return code			*
C************************************************************************
	INTEGER		iarray (*)
C------------------------------------------------------------------------
C*	Read the record.
C
	READ  ( UNIT = lun, REC = irec, IOSTAT = iostat )
     +					( iarray (i), i = 1, lenw )
C
C*	Get GEMPAK file error number.
C
	IF  ( iostat .ne. 0 )  THEN
	    iret = -4
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
