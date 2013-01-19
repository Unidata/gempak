	SUBROUTINE SHN_INTF ( iyr, imo, ida, ihr, imi, id, parcod, idur,
     +			     valu, iqual, irev )
C************************************************************************
C* SHN_INTF								*
C*									*
C* This subroutine creates interface output from observational data	*
C* output by SHEFLIB.  It attempts to accumulate as many SHEFLIB data	*
C* items as possible into a single interface report by checking whether	*
C* the station ID and date-time for the latest observation are the same	*
C* as for the current interface report.  If so, then the new SHEFLIB	*
C* data item is simply appended to the current interface report.	*
C* Otherwise, the existing interface report is converted into BUFR and	*
C* flushed from the decoder, and then the interface arrays are		*
C* re-initialized in order to hold the new SHEFLIB data item.		*
C*									*
C* SHN_INTF ( IYR, IMO, IDY, IHR, IMI, ID, PARCOD, IDUR,			*
C*	     VALU, IQUAL, IREV )					*
C*									*
C* Input parameters:							*
C*	IYR		INTEGER		Year of observation		*
C*	IMO		INTEGER		Month of observation		*
C*	IDY		INTEGER		Day of observation		*
C*	IHR		INTEGER		Hour of observation		*
C*	IMI		INTEGER		Minute of observation		*
C*	ID		CHAR*		Station ID			*
C*	PARCOD		CHAR*		SHEF parameter code array	*
C*	IDUR		INTEGER		SHEF duration code		*
C*	VALU		DOUBLE		Observational data value	*
C*	IQUAL		CHAR*		SHEF quality code pertaining	*
C*					to VALU				*
C*	IREV		INTEGER		SHEF revision code pertaining	*
C*					to VALU				*		
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C* J. Ator/NCEP		07/06	Include shstyp in /INTF/ output.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	CHARACTER	id*8, parcod*8, iqual, cimn*8, ctdu*2
C*
	DOUBLE PRECISION	valu
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C
C*	Ignore any data that is non-observational in nature.
C
	IF ( parcod (4:4) .ne. 'R' ) THEN
	    logmsg = '  non-observational data of type ' //
     +		     parcod(4:4) // ' with element code ' //
     +		     parcod(1:2) // ' for ID ' // id
	    CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*	Ignore any data that is "missing".
C
	rvalu = valu
	IF ( ERMISS ( rvalu ) ) THEN
	    RETURN
	END IF
C
C*	Are the ID and observational date-time the same as for the data
C*	that were stored during the previous call to this subroutine?
C
	IF ( ( civals (1) .ne. id ) .or.
     +	     ( rivals (1) .ne. iyr ) .or.
     +	     ( rivals (2) .ne. imo ) .or.
     +	     ( rivals (3) .ne. ida ) .or.
     +	     ( rivals (4) .ne. ihr ) .or.
     +	     ( rivals (5) .ne. imi ) )  THEN
C
C*	    NO, so convert all accumulated interface data for the
C*	    previous ID and observational date-time into BUFR and then
C*	    re-initialize the interface arrays to hold the current data.
C
	    IF ( nimn .gt. 8 ) THEN
		CALL SHN_IFPT ( 3, ierifp )
                IF ( iflagw .eq. 1 ) THEN
                   CALL SHN_BUFR ( ierbfr )
                ELSE
                   CALL SHN_GEMP ( iergem )
                END IF
	    END IF
	    CALL SHN_IFIV ( ierifi )
C
C*	    Search for this new ID in the station table in order to
C*	    determine its location information.
C
	    CALL DC_BSRC ( id, shstid, nste, ii, iersrh )
	    IF ( ii .eq. 0 )  THEN
		logmsg = id // ' not found in SHEF station table'
		CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg)
		RETURN
	    END IF
C
C*	    Store the new ID, observational date-time and location
C*	    information into the interface arrays.
C
	    cimnem (1) = 'STID'
	    civals (1) = id
	    cimnem (2) = 'STYP'
	    civals (2) = shstyp (ii)
C
	    rimnem (1) = 'YEAR'
	    rivals (1) = iyr
	    rimnem (2) = 'MNTH'
	    rivals (2) = imo
	    rimnem (3) = 'DAYS'
	    rivals (3) = ida
	    rimnem (4) = 'HOUR'
	    rivals (4) = ihr
	    rimnem (5) = 'MINU'
	    rivals (5) = imi
	    rimnem (6) = 'SLAT'
	    rivals (6) = shslat (ii)
	    rimnem (7) = 'SLON'
	    rivals (7) = shslon (ii)
	    rimnem (8) = 'SELV'
	    rivals (8) = shselv (ii)
C
	    nimn = 8
	END IF
C
C*	Determine the time duration and interface mnemonic
C*	pertaining to the current data.
C
	CALL SHN_TDIM  ( idur, parcod(6:6), parcod(1:2),
     +			cimn, td, ctdu, ierdth )
	IF ( ierdth .ne. 0 ) THEN
	    RETURN
	END IF
C
C*	Store the current data into the interface arrays.
C
	rimnem (nimn+1) = 'IDUR' // ctdu
	rivals (nimn+1) = td
	rimnem (nimn+2) = cimn
	rivals (nimn+2) = rvalu
	cimnem (nimn+2) = 'IQUAL'
	civals (nimn+2) = iqual
	rimnem (nimn+3) = 'IREV'
	rivals (nimn+3) = irev
	nimn = nimn + 3
C
C*	Is there any more room in the interface arrays?
C
	IF ( ( nimn + 3 ) .gt. MXIMN ) THEN
C
C*	    NO, so go ahead now and convert all existing interface
C*	    output into BUFR, then re-initialize the interface arrays
C*	    in advance of the next call to this subroutine.
C
	    logmsg = '  No more room in interface arrays,' //
     +		     ' so will flush BUFR output now and reset.'
	    CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg)
	    CALL SHN_IFPT ( 3, ierifp )

            IF ( iflagw .eq. 1 ) THEN
	        CALL SHN_BUFR ( ierbfr )
            ELSE
                CALL SHN_GEMP ( iergem )
            END IF

	    CALL SHN_IFIV ( ierifi )
	END IF
C*
	RETURN
	END
