	SUBROUTINE FL_DSOP  ( filnam, irecsz, lun, iret )
C************************************************************************
C* FL_DSOP								*
C* 									*
C* This subroutine opens an existing direct access file for shared, 	*
C* write access.  It returns a logical unit number to be used to	*
C* access the file.							*
C*									*
C* This subroutine is provided so that real time data ingest programs	*
C* can update a file while other programs have the file open for	*
C* read access.  FL_DOPN should be used to open files for write		*
C* access by non-real time programs.					*
C* 									*
C* FL_DSOP  ( FILNAM, IRECSZ, LUN, IRET )				*
C* 									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*	IRECSZ		INTEGER		Record length in words		*
C* 									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					<>0 = GEMPAK file error 	*
C**									*
C* Log:									*
C* M. desJardins/NMC	 8/94	Call FL_DOPN since they are the same	*
C*				on UNIX systems				*
C* S. Jacobs/NMC	 8/94	Fixed error in call to FL_DOPN		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
C*	Call FL_DOPN since these two subroutines are identical on UNIX
C*	systems.
C
	CALL FL_DOPN  ( filnam, irecsz, .true., lun, iret )
C*
	RETURN
	END
