	SUBROUTINE FL_WRIT ( lun, irec, lenw, iarray, iret )
C************************************************************************
C* FL_WRIT								*
C* 									*
C* This subroutine writes a record to a direct access file.  		*
C* 									*
C* FL_WRIT  ( LUN, IREC, LENW, IARRAY, IRET )				*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*	IREC		INTEGER		Record number			*
C*	LENW		INTEGER		Record length in words		*
C*	IARRAY (LENW)	INTEGER		Data record			*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = cannot write to file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C* S. Maxwell/GSC	12/96 	Modified return code; removed FL_IRET	*
C************************************************************************
	INTEGER		iarray (*)
C------------------------------------------------------------------------
	knt  = 1
C
C*	Write the record.
C
	WRITE  ( UNIT = lun, REC = irec, IOSTAT = iostat )
     +					( iarray (i), i = 1, lenw )
C
C*	Get the GEMPAK file error number.
C
	IF  ( iostat .ne. 0 ) THEN
	    iret = -5
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
