	SUBROUTINE SN_TTIM  ( isnfln, dattim, iret )
C************************************************************************
C* SN_TTIM								*
C*									*
C* This subroutine sets the time in a sounding file.  SN_TSTN must	*
C* be called before this subroutine is called.  Data for this time	*
C* can be returned or written by calling SN_RDAT or SN_WDAT,		*
C* respectively.							*
C*									*
C* SN_TTIM  ( ISNFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				   	   0 = normal return		*
C*				  	  -4 = file not open		*
C*					 -12 = time not found		*
C*					 -20 = station not set		*
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
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that station has been set.
C
	IF  ( .not. stnset ( isnfln ) ) THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Reset search  conditions.
C
	CALL SN_BEGS  ( isnfln, ier )
C
C*	Delete existing search condition.
C
	CALL DM_DCSR  ( isnfln, ier )
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM ( dattim, lowval (1), lowval (2), ier )
	DO i= 1, 2
	    ihival (i) = lowval (i)
	END DO
C
C*	Establish search.
C
	CALL DM_CSRC  ( isnfln, .true., 2, keynam, lowval, ihival, ier )
C
C*	Find next time meeting search criteria.
C
	CALL SN_TNXT  ( isnfln, dattim, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -12
	END IF
C
C*	Delete search condition.
C
	CALL DM_DCSR  ( isnfln, ier )
C*
	RETURN
	END
