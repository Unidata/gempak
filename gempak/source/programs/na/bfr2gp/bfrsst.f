	SUBROUTINE BFRSST ( lunsn, lunsf, ship, stid, istnm,
     +			    slat, slon, selv,
     +                      dattim, dtmlst, sidlst, isnlst, iret )
C************************************************************************
C* BFRSST								*
C*									*
C* This subroutine sets the station and time for both the sounding	*
C* and surface data output files.					*
C*									*
C* BFRSST  ( LUNSN, LUNSF, SHIP, STID, ISTNM, SLAT, SLON, SELV, DATTIM,	*
C*           DTMLST, SIDLST, ISNLST, IRET )				*
C*									*
C* Input parameters:							*
C*	LUNSN		INTEGER		Sounding file number		*
C*	LUNSF 		INTEGER		Surface file number		*
C*	SHIP		LOGICAL		Flag for ship surface file	*
C*	STID		CHAR*		Station ID			*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	DATTIM		CHAR*		Date/time			*
C*									*
C* Input and output parameters:						*
C*	DTMLST		CHAR*		Previous date/time		*
C*	SIDLST		CHAR*		Previous station ID		*
C*	ISNLST		INTEGER		Previous station number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-23 = Snd time cannot be set	*
C*					-24 = Sfc time cannot be set	*
C*					-25 = Snd station cannot be set *
C*					-26 = Sfc station cannot be set *
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C* K. Brill/EMC		 2/97	Added SHIP				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, dattim, dtmlst, sidlst
	LOGICAL		ship
C*
	CHARACTER	stat*2, coun*2
	DATA		stat, coun / 2 * '  '/
C-----------------------------------------------------------------------
	iret = 0
	IF ( ship ) RETURN
	IF ( dattim .ne. dtmlst ) THEN
C
C*          If time is not already in files, add time.
C
	    IF ( lunsn .ne. 0 ) THEN
            	CALL SN_FTIM  ( lunsn, dattim, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SN_ATIM  ( lunsn, dattim, ier )
                    IF  ( ier .ne. 0 )  THEN
                    	iret = -23
                    	RETURN
		    ELSE
                	CALL SN_FTIM  ( lunsn, dattim, ier )
                    END IF
            	END IF
	    END IF
	    IF ( lunsf .ne. 0 ) THEN
            	CALL SF_FTIM  ( lunsf, dattim, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SF_ATIM  ( lunsf, dattim, ier )
                    IF  ( ier .ne. 0 )  THEN
                    	iret = -24
                    	RETURN
		    ELSE
                    	CALL SF_FTIM  ( lunsf, dattim, ier )
                    END IF
            	END IF
	    END IF
	    dtmlst = dattim
	END IF
C*
 	IF ( ( isnlst .ne. istnm .or. istnm .eq. IMISSD ) .or.
     +	     ( sidlst .ne. stid .or. stid .eq. ' ' ) ) THEN
C
C*	    If station is not already in files, add it.
C
	    IF ( lunsn .ne. 0 ) THEN
            	CALL SN_FSTN  ( lunsn, stid, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SN_ASTN  ( lunsn, 1, stid, istnm, slat,
     +                              slon, selv, stat, coun, n, ier )
                    IF  ( ier .ne. 0 )  THEN
		    	iret = -25
		    	RETURN
                    ELSE
                    	CALL SN_FSTN  ( lunsn, stid, ier )
                    END IF
            	END IF
	    END IF
	    IF ( lunsf .ne. 0 ) THEN
            	CALL SF_FSTN  ( lunsf, stid, ier )
            	IF  ( ier .ne. 0 )  THEN
		    ispri = 1
                    CALL SF_ASTN  ( lunsf, 1, stid, istnm, slat,
     +                              slon, selv, stat, coun, ispri,
     +				    n, ier )
                    IF  ( ier .ne. 0 )  THEN
		    	iret = -26
		    	RETURN
                    ELSE
                    	CALL SF_FSTN  ( lunsf, stid, ier )
                    END IF
            	END IF
	    END IF
	    sidlst = stid
	    isnlst = istnm
	END IF
C*
	RETURN
	END

