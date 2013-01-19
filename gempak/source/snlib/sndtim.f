	SUBROUTINE SN_DTIM  ( isnfln, dattim, iret )
C************************************************************************
C* SN_DTIM								*
C*									*
C* This subroutine deletes a time from a sounding file.  All the data	*
C* corresponding to the time will be deleted along with the headers	*
C* storing the time.							*
C*									*
C* SN_DTIM  ( ISNFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*					  -4 = file not open		*
C*				  	 -15 = delete error		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*) 	dattim
C*
	INTEGER		lowval (2), ihival (2)
	CHARACTER	keynam (2)*4
	DATA		keynam /'DATE', 'TIME'/
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF  (iret .ne. 0 ) RETURN
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM ( dattim, lowval (1), lowval (2), ier )
	DO i= 1, 2
	    ihival (i) = lowval (i)
	END DO
C
C*	Delete row/column.
C
	CALL DM_DALL  ( isnfln, 2, keynam, lowval, ihival, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -15
	END IF
C*
	RETURN
	END
