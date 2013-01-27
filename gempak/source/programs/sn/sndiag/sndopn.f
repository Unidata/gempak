	SUBROUTINE SNDOPN  ( snfile, snfcur, timstn, parms, nparms,
     +			     isnfln, newfil, iret )
C************************************************************************
C* SNDOPN								*
C*									*
C* This subroutine opens the sounding dataset and checks that it	*
C* contains the same parameters as the edit file.  If SNFILE does	*
C* not exist, a new sounding file with the given parameters is		*
C* created.								*
C*									*
C* SNDOPN  ( SNFILE, SNFCUR, TIMSTN, PARMS, NPARMS, ISNFLN, NEWFIL, 	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Sounding file name		*
C*	SNFCUR		CHAR*		Current sounding file		*
C*	TIMSTN		CHAR*		# time, stations in new file	*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	NPARMS 		INTEGER		Number of parameters		*
C*									*
C* Output parameters:                                                   *
C*	ISNFLN		INTEGER		Sounding file number		*
C*	NEWFIL		LOGICAL		New file created flag		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					  7 = error creating file	*
C*					 -8 = different parms		*
C*					 -9 = invalid vertical coord	*
C*					-10 = error opening file	*
C*					-11 = unmerged dataset		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Copied from SNEOPN		*
C* J. Whistler/SSAI	 4/93		Cleaned up			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfcur, timstn, parms (*)
	LOGICAL		newfil
C*
	CHARACTER	vparm*4, dsparm (MMPARM)*4
	LOGICAL		exist, mrgdat
C*
	INTEGER		iarr (2)
	EQUIVALENCE	( iarr (1), ntime ) , ( iarr (2), nstns )
C------------------------------------------------------------------------
	iret   = 0
	newfil = .false.
C
C*	Check to see if file requested is current file.
C
	IF  ( (snfile .eq. snfcur)  .and. (snfile .ne. ' ') ) RETURN
C
C*	Find vertical coordinate.
C
	CALL LV_CORD  ( parms (1), vparm, ivert, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	First check to see if the file exists.
C
	INQUIRE  ( FILE = snfile, EXIST = exist )
C
C*	If the file exists, open and check parameters.
C
	IF  ( exist )  THEN
	    CALL SN_OPNF  ( snfile, .true., isnfln, ifl, mparms,
     +			    dsparm, jvert, mrgdat, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SN', iret, snfile, ier )
		iret = -10
	      ELSE IF  ( ivert .ne. jvert )  THEN
		iret = -8
	      ELSE IF  ( .not. mrgdat )  THEN
		iret = -11
	      ELSE IF  ( mparms .ne. nparms )  THEN
		iret = -8
	      ELSE 
		DO  i = 1, nparms
		    IF  ( parms (i) .ne. dsparm (i) )  iret = -8
		END DO
	    END IF
C
C*	    Otherwise, create a new file.
C
	  ELSE
	    CALL ST_ILST  ( timstn, '/', 0, 2, iarr, n, ier )
C
C*	    Check for errors in ntime and nstns.
C
	    IF  ( ntime .le. 0 )  THEN
		iret = -12
	      ELSE IF  ( nstns .le. 0 )  THEN
		iret = -13
	      ELSE
		newfil = .true.
		CALL SN_CREF  ( snfile, MFRAOB, nparms, parms, nstns,
     +				ntime, .false., isc, iof, ibt,
     +				.true., isnfln, iret )
		IF  ( iret .ne. 0 )  THEN
		    iret = -7
		END IF
	    END IF
	END IF
	snfcur = snfile
C*
	RETURN
	END
