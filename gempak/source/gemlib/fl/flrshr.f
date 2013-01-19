	SUBROUTINE FL_RSHR  ( lun, irec, lenw, iarray, iret )
C************************************************************************
C* FL_RSHR								*
C* 									*
C* This subroutine reads a record from a direct access file.  On a VMS	*
C* system, if the record is locked by another user, 30 tries to open the*
C* file will be attempted at 1-second intervals.  This subroutine is	*
C* meant to be called when a file is opened for shared, write access.  	*
C* As each record is read, it is written back to the file in order to 	*
C* prevent records from being locked on VMS systems.  This subroutine	*
C* should not be necessary on UNIX systems.				*
C* 									*
C* FL_RSHR  ( LUN, IREC, LENW, IARRAY, IRET )				*
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
C*					 52 = locked record		*
C*					<>0 = GEMPAK file error 	*
C**									*
C* Log:									*
C* M. desJardins/NMC	 8/94	Call FL_READ; identical on UNIX		*
C************************************************************************
	INTEGER		iarray (*)
C------------------------------------------------------------------------
C*	Call FL_READ to read record since it is identical with this
C*	subroutine on UNIX systems.
C
	CALL FL_READ  ( lun, irec, lenw, iarray, iret )
C*
	RETURN
	END
