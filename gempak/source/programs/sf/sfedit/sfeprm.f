	SUBROUTINE SFEPRM  ( sfefil, prmdst, npmdst, stndrd, lunedt,
     +			     parms, nparm,  iploc,  iret )
C************************************************************************
C* SFEPRM								*
C*									*
C* This subroutine opens the EDIT file and checks the parameters in	*
C* that file.								*
C*									*
C* SFEPRM  ( SFEFIL, PRMDST, NPMDST, STNDRD, LUNEDT, PARMS, NPARM,	*
C*	     IPLOC, IRET )						*
C*									*
C* Input parameters:							*
C*	SFEFIL		CHAR*		Edit file name			*
C*	PRMDST (NPMDST)	CHAR*		Data set parameters		*
C*	NPMDST		INTEGER		Number of parameters		*
C*	STNDRD		LOGICAL		Standard file flag		*
C*									*
C* Output parameters:							*
C*	LUNEDT		INTEGER		LUN for edit file		*
C*	PARMS  (NPARM)	CHAR*		Parameters to output		*
C*	NPARM		INTEGER		Number of output parameters	*
C*	IPLOC  (NPARM)	INTEGER		Pointers to parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -3 = file not opened		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* J. Whistler/SSAI      1/91   Replaced ST_C2C with ST_CLST added a	*
C*				LOGICAL more to test if additional	*
C*				PARMs are located on next record	*
C* P. Bruehl/Unidata     1/94   Added ieloc edit file pointers          *
C* P. Bruehl/Unidata     1/94   Added check for slat/slon and illoc as  *
C*                              special case for ship data              *
C* S. Jacobs/NMC	10/94	Rewrote Ship data case			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	prmdst (*), parms (*), sfefil
	INTEGER		iploc  (*)
	LOGICAL		stndrd
C*
	CHARACTER	record*132, pname (20)*4, tmprec*132
	LOGICAL		found, more, havlat, havlon
C------------------------------------------------------------------------
	iret  = 0
C
C*	If the file is non-standard, leave space for SLAT and SLON.
C
	IF  ( stndrd )  THEN
	    nparm  = 0
	ELSE
	    nparm  = 2
	    havlat = .false.
	    havlon = .false.
	END IF
C
C*	Open the file.
C
	CALL FL_SOPN  ( sfefil, lunedt, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, sfefil, iret )
	    iret   = -3
	    lunedt = 0
	    RETURN
	END IF
C
C*	Loop through records looking for the record containing
C*	parameter names.
C
	ier   = 0
	found = .false.
	DO WHILE  ( ier .eq. 0 )
	    READ   ( lunedt, 10, IOSTAT= ier )  record
10	    FORMAT ( A )
	    CALL ST_LCUC  ( record, record, ier2 )
C
C*	    Check for "PARM".
C
	    IF  ( ier .eq. 0 )  THEN
		ip = INDEX  ( record, 'PARM' )
		IF  ( ip .gt. 0 )  THEN
		    ieq = INDEX  ( record, '=' )
		    IF  ( ieq .gt. 0 )  THEN
			found = .true.
			ier   = -1
		    END IF
		END IF
	    END IF
	END DO
C
C*	If PARM was not found, close file and return error.
C
	IF  ( .not. found )  THEN
	    CALL FL_CLOS  ( lunedt, ier )
	    lunedt = 0
	    iret   = -5
	    RETURN
	END IF
	tmprec = record ( ieq+1 : )
	record = tmprec
C
C*	Get parameter names from this record.
C
	DO WHILE  ( record .ne. ' ')
C
C*	    Remove blanks from this record.
C
	    CALL ST_RMBL ( record, record, irlen, ier )
C
C*	    Check to see if additional parameters are on next record
C
	    IF ( ( irlen .gt. 0 ) .and.
     +		 ( record ( irlen : irlen ) .eq. ';' ) ) THEN
		more = .true.
	      ELSE
		more = .false.
	    ENDIF
C
C*	    Break record into substrings.
C
	    CALL ST_CLST  ( record, ';', ' ', 20, pname, npnam, ier )
C
C*	    Check that there was something in the record.
C
	    IF  ( npnam .eq. 0 )  THEN
		record = ' '
C
C*		Add these parameters to the current list.
C
	      ELSE
		DO  i = 1, npnam
C
C*		    Check that there are not too many parameters.
C
		    IF  ( nparm .ge. MMPARM )  THEN
			ier = -6
			CALL FL_CLOS  ( lunedt, ier )
			lunedt = 0
			iret   = -5
			RETURN
		      ELSE
C
C*			Check that parameters are in the GEMPAK
C*			file data set.
C
			CALL ST_FIND  ( pname (i), prmdst, npmdst,
     +					ipos, ier )
			IF  ( ipos .eq. 0 )  THEN
C
C*			    Check for SLAT and SLON in the edit file.
C
			    IF  ( ( .not. stndrd ) .and.
     +				  ( pname (i) .eq. 'SLAT' ) .and.
     +				  ( i .eq. 1 ) )  THEN
				iploc ( 1 ) = 1
				parms ( 1 ) = pname (i)
				havlat = .true.
			      ELSE IF  ( ( .not. stndrd ) .and.
     +				  	 ( pname (i) .eq. 'SLON' ) .and.
     +					 ( i .eq. 2 ) )  THEN
				iploc ( 2 ) = 2
				parms ( 2 ) = pname (i)
				havlon = .true.
			      ELSE
				iret = -8
				CALL ER_WMSG  ( 'SFEDIT', iret,
     +						pname (i), ier )
				nparm = 0
			    END IF
			  ELSE
			    nparm = nparm + 1
			    parms ( nparm ) = pname (i)
			    iploc ( nparm ) = ipos
			END IF
		    END IF
		END DO
C
C*		Read in next record if necessary.
C
                IF ( more ) THEN
		    READ   ( lunedt, 10, IOSTAT= ier )  record
		    CALL ST_LCUC  ( record, record, ier2 )
		    IF  ( ier .ne. 0 )  THEN
		        ier = -5
		        CALL FL_CLOS  ( lunedt, ier )
		        lunedt = 0
		        iret   = -5
		        RETURN
		    END IF
		  ELSE
		    record = ' '
		END IF
	    END IF
	END DO
C
C*	Return an error if this non-standard data and the lats/lons
C*	are missing.
C
	IF  ( ( .not. stndrd ) .and. 
     +	      ( ( .not. havlat ) .or. ( .not. havlon ) ) )  iret = -10
C*
	RETURN
	END
