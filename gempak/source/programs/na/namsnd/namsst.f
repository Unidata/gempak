	SUBROUTINE NAMSST ( lunsn, lunsf, stid, istnm, slat, slon, selv,
     +                      dattim, dtmlst, sidlst, isnlst, iret )
C************************************************************************
C* NAMSST								*
C*									*
C* This subroutine sets the station and time for both the sounding	*
C* and surface data output files.					*
C*									*
C* NAMSST  ( LUNSN, LUNSF, STID, ISTNM, SLAT, SLON, SELV, DATTIM,	*
C*           DTMLST, SIDLST, ISNLST, IRET )				*
C*									*
C* Input parameters:							*
C*	LUNSN		INTEGER		Sounding file number		*
C*	LUNSF (2)	INTEGER		Surface file number		*
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
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* K. Brill/NMC		10/94	Set time in auxiliary sfc file		*
C* D. Kidwell/NCEP	12/98	SNMSST -> NAMSST                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, dattim, dtmlst, sidlst
C*
	INTEGER		lunsf (2)
	CHARACTER	stnid*8, stat*2, coun*2
	DATA		stat, coun / 2 * '  '/
C-----------------------------------------------------------------------
	iret = 0
	IF ( dattim .ne. dtmlst ) THEN
C
C*          If time is not already in files, add time.
C
	    IF ( lunsn .ne. 0 ) THEN
            	CALL SN_FTIM  ( lunsn, dattim, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SN_ATIM  ( lunsn, dattim, ier )
                    IF  ( ier .ne. 0 )  THEN
                    	iret = -16
                    	RETURN
		    ELSE
                	CALL SN_FTIM  ( lunsn, dattim, ier )
                    END IF
            	END IF
	    END IF
	    IF ( lunsf (1) .ne. 0 ) THEN
            	CALL SF_FTIM  ( lunsf (1), dattim, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SF_ATIM  ( lunsf (1), dattim, ier )
                    IF  ( ier .ne. 0 )  THEN
                    	iret = -17
                    	RETURN
		    ELSE
                    	CALL SF_FTIM  ( lunsf (1), dattim, ier )
                    END IF
            	END IF
	    END IF
	    IF ( lunsf (2) .ne. 0 ) THEN
            	CALL SF_FTIM  ( lunsf (2), dattim, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SF_ATIM  ( lunsf (2), dattim, ier )
                    IF  ( ier .ne. 0 )  THEN
                    	iret = -17
                    	RETURN
		    ELSE
                    	CALL SF_FTIM  ( lunsf (2), dattim, ier )
                    END IF
            	END IF
	    END IF
	    dtmlst = dattim
	END IF
C*
 	IF ( isnlst .ne. istnm ) THEN
C
C*	    If station is not already in files, add it.
C
	    CALL ST_INCH ( istnm, stnid, ier )
	    IF ( lunsn .ne. 0 ) THEN
            	CALL SN_FSTN  ( lunsn, stnid, ier )
            	IF  ( ier .ne. 0 )  THEN
                    CALL SN_ASTN  ( lunsn, 1, stid, istnm, slat,
     +                              slon, selv, stat, coun, n, ier )
                    IF  ( ier .ne. 0 )  THEN
		    	iret = -18
		    	RETURN
                    ELSE
                    	CALL SN_FSTN  ( lunsn, stnid, ier )
                    END IF
            	END IF
	    END IF
	    IF ( lunsf (1) .ne. 0 ) THEN
            	CALL SF_FSTN  ( lunsf (1), stnid, ier )
            	IF  ( ier .ne. 0 )  THEN
		    ispri = 1
                    CALL SF_ASTN  ( lunsf (1), 1, stid, istnm, slat,
     +                              slon, selv, stat, coun, ispri,
     +				    n, ier )
                    IF  ( ier .ne. 0 )  THEN
		    	iret = -19
		    	RETURN
                    ELSE
                    	CALL SF_FSTN  ( lunsf (1), stnid, ier )
                    END IF
            	END IF
	    END IF
	    IF ( lunsf (2) .ne. 0 ) THEN
            	CALL SF_FSTN  ( lunsf (2), stnid, ier )
            	IF  ( ier .ne. 0 )  THEN
		    ispri = 1
                    CALL SF_ASTN  ( lunsf (2), 1, stid, istnm, slat,
     +                              slon, selv, stat, coun, ispri,
     +				    n, ier )
		    IF  ( ier .ne. 0 )  THEN
		    	iret = -19
		    	RETURN
                    ELSE
                    	CALL SF_FSTN  ( lunsf (2), stnid, ier )
                    END IF
            	END IF
	    END IF
	    sidlst = stid
	    isnlst = istnm
	END IF
C*
	RETURN
	END

