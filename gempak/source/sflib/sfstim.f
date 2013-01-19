	SUBROUTINE SF_STIM  ( isffln, dattim, iret )
C************************************************************************
C* SF_STIM								*
C*									*
C* This subroutine sets the time in a surface file.  All later		*
C* station searches will return stations corresponding to this time.	*
C*									*
C* SF_STIM  ( ISFFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				   	   0 = normal return		*
C*				  	  -3 = file not open		*
C*					 -11 = time not in file		*
C*					 -17 = time not set		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 4/90	Single station search			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*) 	dattim
C*
	CHARACTER	keynam (2)*4
	INTEGER		lowval (2), ihival (2)
	LOGICAL		timflg
	DATA		keynam /'DATE', 'TIME'/
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Search for time using quick search if there is only one station
C*	to be found.
C
	IF  ( onestn (isffln) )  THEN
	    CALL SF_FTIM  ( isffln, dattim, iret )
	    onefnd (isffln) = .false.
	    RETURN
	END IF
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM  ( dattim, lowval (1), lowval (2), ier )
	DO  i= 1, 2
	    ihival (i) = lowval (i)
	END DO
C
C*	Check that time is in file.
C
	CALL SF_CTIM  ( isffln, lowval (1), lowval (2), timflg, ier )
	IF  ( .not. timflg )  THEN
	    iret = -11
	    RETURN
	END IF
C
C*	Reset search.
C
	CALL SF_BEGS  ( isffln, ier )
C
C*	Establish search.
C
	CALL DM_PSRC  ( isffln, 2, keynam, lowval, ihival, ier )
	IF  ( ier .eq. 0 )  THEN
	    timset (isffln) = .true.
	    curtim (isffln) = dattim
	  ELSE
	    timset (isffln) = .false.
	    curtim (isffln) = ' '
	    iret = -17
	END IF
C
C*	Reset current row and column.
C
	krow (isffln)   = 0
	kcol (isffln)   = 0
	stnset (isffln) = .false.
	ftmset (isffln) = .false.
	curstn (isffln) = ' '
C*
	RETURN
	END
