	SUBROUTINE DA_OPEN ( filnam, iflno, lun, iret )
C************************************************************************
C* DA_OPEN								*
C*									*
C* This function opens and reads the configuration file used to define	*
C* DM-like values for a non-GEMPAK data source.				*
C*									*
C* DA_OPEN ( FILNAM, LUN, IRET )					*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	IFLNO		INTEGER		GEMPAK file number		*
C*									*
C* Output parameters:							*
C*	LUN		INTEGER		File logical unit number	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/13	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	dirnam*(MXFLSZ), basnam*(MXFLSZ),
     +			fname*(MXFLSZ), fullname*(MXFLSZ)
	LOGICAL		exist
C------------------------------------------------------------------------
	iret = 0
C
C*	Find the path to the configuration file
C
	CALL FL_PATH ( filnam, dirnam, basnam, ier )
	CALL FL_TINQ ( basnam, 'data-access', exist, fname, ier )
	CALL SS_ENVR ( fname, fullname, ier )
	CALL ST_NULL ( fullname, fullname, lenf, ier )
C
C*	Read the contents of the configuration file into the structure
C
	CALL DA_READXML ( fullname, iflno, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG ( 'DA', ierr, fullname, ier )
	END IF
C
C*	Open the table and pass the unit number back to the DM library
C
	CALL FL_TBOP ( basnam, 'data-access', lun, ier )
C*
	RETURN
	END
