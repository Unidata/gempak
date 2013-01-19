	SUBROUTINE MS_OPNC ( climtm, iflsrc, stntbl, iadstn, maxtim,
     +			     iflno, iret ) 
C************************************************************************
C* MS_OPNC								*
C*									*
C* This subroutine gets the file number for the climatology file.  If   *
C* necessary, a new file will be opened.                                *
C*									*
C* MS_OPNC ( CLIMTM, IFLSRC, STNTBL, IADSTN, MAXTIM, IFLNO, IRET )      *
C*									*
C* Input parameters:							*
C*	CLIMTM  	CHAR*	 	GEMPAK climatology time         *
C*	IFLSRC		INTEGER		File source			*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Max num of additional stations	*
C*	MAXTIM		INTEGER		Max num of times for land data	*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		Climate file number             *
C*	IRET		INTEGER		Return code			*
C*                                        0 = normal return             *
C*					 -4 = climo file not found      *
C*                                       -6 = climo file open error     *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00	From MM_OPNC                            *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	climtm, stntbl
C*
	CHARACTER	filnam*72, parms(MMPARM)*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the path to the climatology file.
C
	CALL FL_MFIL ( 'climo', climtm, filnam, ierr )
	IF ( ierr .ne. 0 ) THEN
	    CALL DC_WLOG ( 4, 'FL', ierr, climtm, ier )
	    iret = -4
	    RETURN
	END IF
C
C* 	Get the unit number.  Open a new file if necessary.
C
	CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, maxtim, iflno,
     +		       nparm, parms, ierr )
C
C*	Check that the file was opened properly.
C
	IF ( ierr .ne. 0 ) THEN
	    CALL DC_WLOG ( 0, 'SF', ierr, filnam, ier )
	    iret = -6
	END IF
C*
	RETURN
	END
