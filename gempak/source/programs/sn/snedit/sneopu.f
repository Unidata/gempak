	SUBROUTINE SNEOPU  ( snfile, timstn, isnfln, newfil, iret )
C************************************************************************
C* SNEOPU								*
C*									*
C* This subroutine opens an unmerged sounding dataset. 			*
C* If SNFILE does not exist, a new sounding file with the given 	*
C* parameters is created.						*
C*									*
C* SNEOPU  ( SNFILE, TIMSTN, ISNFLN, NEWFIL, IRET )			*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*	 	Sounding file name		*
C*      TIMSTN		CHAR*           # Times, stations in new file   *
C*									*
C* Output parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	NEWFIL		LOGICAL		New file created flag		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = error creating file	*
C*					 -8 = different parms		*
C*					 -9 = invalid vertical coord	*
C*					-10 = error opening file	*
C*					-20 = merged dataset		*
C*					-12 = NTIME is non positive	*
C*					-13 = NSTNS is non positive	*
C**									*
C* Log:									*
C* S. Schotz/GSC	12/89						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, timstn
	LOGICAL		newfil
C*
	CHARACTER	dsparm (MMPARM)*4
	LOGICAL		exist, mrgdat
C*
	INTEGER 	iarr (2)
	EQUIVALENCE 	( iarr (1), ntime ) , ( iarr (2), nstns )
C*
C-------------------------------------------------------------------------
	iret   = 0
	newfil = .false.
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
            IF ( iret .ne. 0) THEN
		CALL ER_WMSG ( 'SN', iret, snfile, ier )
		iret = -10
		RETURN
	    ELSE IF  ( mrgdat )  THEN
		iret = -20
		RETURN
            END IF
C
C*	    Otherwise, create a new file.
C
        ELSE
	    CALL ST_ILST ( timstn, '/', 0, 2, iarr, n, ier )
C
C*	    Check for errors in ntime and nstns.
C
	    IF ( ntime .le. 0 ) THEN
		iret = -12
	    ELSE IF ( nstns .le. 0 ) THEN
		iret = -13
	    ELSE
                newfil = .true.
	        CALL SN_CRUA ( snfile, MFRAOB, 3, nstns, ntime, 
     +                      .false., .true., .true., isnfln, iret )
                IF  ( iret .ne. 0 )  THEN
	           iret = -7
	        END IF
            END IF
	END IF
C*
	RETURN
	END
