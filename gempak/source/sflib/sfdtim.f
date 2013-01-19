	SUBROUTINE SF_DTIM  ( isffln, dattim, iret )
C************************************************************************
C* SF_DTIM								*
C*									*
C* This subroutine deletes a time from a surface file.  All the data	*
C* corresponding to the time will be deleted along with the header	*
C* storing the time.							*
C*									*
C* SF_DTIM  ( ISFFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*					  -3 = file not open		*
C*				  	 -13 = delete error		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
C*
	CHARACTER*(*) 	dattim
C*
	CHARACTER	keynam (2)*4
	INTEGER		lowval (2), ihival (2)
	DATA		keynam /'DATE', 'TIME'/
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM  ( dattim, lowval (1), lowval (2), ier )
	DO  i = 1, 2
	    ihival (i) = lowval (i)
	END DO
C
C*	Delete row/column.
C
	CALL DM_DALL  ( isffln, 2, keynam, lowval, ihival, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', ier, dattim, ierr )
	    iret = -13
	END IF
C*
	RETURN
	END
