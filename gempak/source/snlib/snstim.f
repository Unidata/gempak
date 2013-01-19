	SUBROUTINE SN_STIM  ( isnfln, dattim, iret )
C************************************************************************
C* SN_STIM								*
C*									*
C* This subroutine sets the time in a sounding file.  All later		*
C* station searches will return stations corresponding to this time.	*
C*									*
C* SN_STIM  ( ISNFLN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		GEMPAK date/time		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				   	   0 = normal return		*
C*				  	  -4 = file not open		*
C*					 -12 = time not in file		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*) 	dattim
C*
	CHARACTER	keynam (2)*4
	INTEGER		lowval (2), ihival (2)
	LOGICAL		timflg
	DATA		keynam /'DATE', 'TIME'/
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 )  RETURN
C
C*	Break the time into 2 integers.
C
	CALL TI_IDTM ( dattim, lowval (1), lowval (2), ier )
	DO  i= 1, 2
	    ihival (i) = lowval (i)
	END DO
C
C*	Check that time is in file.
C
	CALL SN_CTIM  ( isnfln, lowval (1), lowval (2), timflg, ier )
	IF  ( .not. timflg )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Reset search.
C
	CALL SN_BEGS ( isnfln, ier )
C
C*	Establish search.
C
	CALL DM_PSRC ( isnfln, 2, keynam, lowval, ihival, ier )
C
C*	Set flags indicating type of search.
C
	timset ( isnfln ) = .true.
	curtim ( isnfln ) = dattim
C
C*	Reset current row and column and turn off other searches.
C
	krow   ( isnfln ) = 0
	kcol   ( isnfln ) = 0
	stnset ( isnfln ) = .false.
	ftmset ( isnfln ) = .false.
	curstn ( isnfln ) = ' '
	icrstn ( isnfln, 1 ) = 0
	icrstn ( isnfln, 2 ) = 0
C*
	RETURN
	END
