	SUBROUTINE AF_DARP  ( report, lenr, cicli, iret )
C************************************************************************
C* AF_DARP								*
C*									*
C* This subroutine decodes an AIREP report.				*
C*									*
C* AF_DARP  ( REPORT, LENR, CICLI, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		AIREP report 			*
C*	LENR		INTEGER		Length of REPORT 		*
C*	CICLI		CHAR*		Bulletin originator 		*
C*									*
C* Output parameters:							*
C*	CIVALS (ICACID)	CHAR*		Aircraft flight identifier	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Remove calls to ERRRPT 			*
C* J. Ator/NP12		03/97	Reject reports where longitude direction*
C*				is not at end of original report group 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/98	Initialize rpids, lrpids via DATA stmts	*
C* D. Kidwell/NCEP	 7/99	Removed irptr from calling sequence     *
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report, cicli
C*
	PARAMETER	( NRPIDS = 2 )
C*
	CHARACTER	rpids ( NRPIDS )*3, field*(MXLENF)
C*
	INTEGER		lrpids ( NRPIDS )
C*
	DATA		rpids
     +		    / 'ARP', 'ARS' /
	DATA		lrpids
     +		    / 3, 3 /
C-----------------------------------------------------------------------
	iret  = 0
	irptr = 1
C
C*	Check if there is an 'ARP' or 'ARS' indicator to denote
C*	the start of the report.  If so, then position the report
C*	pointer directly after the indicator.
C
	CALL ST_NXTS  ( report, irptr, ( irptr + 15 ), rpids, lrpids,
     +			NRPIDS, ipt1, irpid, iernxt )
	IF  ( iernxt .eq. 0 )  THEN
	    irptr = irptr + ipt1 + ( lrpids ( irpid ) ) - 1
	END IF
C
C*	Get the aircraft flight identifier group.
C
	CALL AF_GFLD  ( report, lenr, irptr, field, lenf, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Store the aircraft flight identifier.
C
	IF  ( lenf .le. 8 )  THEN
	    lacid = lenf
	ELSE
	    lacid = 8
	END IF
	civals ( icacid ) = field ( 1 : lacid )
C
C*	Break apart the remainder of the report into groups of
C*	"like-type" in order to facilitate decoding.
C
	CALL AF_BKGP  ( report ( irptr : lenr ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Locate, decode, and store the latitude data from within the
C*	first 5 "like-type" groups.
C
	isdx = 1
	CALL AF_ALAT  ( isdx, 5, islat, ielat, ieralt )
C
C*	Locate, decode, and store the longitude data from within the
C*	first 5 "like-type" groups.
C
	isdx = 1
	CALL AF_ALON  ( isdx, 5, islon, ielon, ieraln )
C
C*	Make sure that the longitude direction indicator marks the
C*	end of an original report group.
C 
	IF  ( ( ielon .ne. IMISSD ) .and.
     +		( ( ielon + 1 ) .le. nflds ) )  THEN
	    IF  ( irfnsf ( ielon ) .eq. irfnsf ( ielon + 1 ) )  THEN
		logmsg = 'Longitude direction does not end a group'
		CALL DC_WLOG  ( 2, 'AF', 1, logmsg, ierwlg )
		RETURN
	    END IF
	END IF
C
	IF  ( ( ielat .eq. IMISSD ) .and. ( ielon .eq. IMISSD ) )  THEN
C
C*	    Search up through the first 3 original report groups
C*	    looking for a waypoint identifier.
C
	    ierwpt = -1
	    irfn = 1
	    DO WHILE  (  ( irfn .le. 3 ) .and.
     +			  ( ierwpt .eq. (-1) )  )
C
C*		Reconstruct the (irfn)th original report group.
C
		CALL AF_ARFG  ( irfn, field, lenf, isnf, ienf, ierrfg )
		IF  ( ierrfg .eq. 0 )  THEN
C
C*		    Check if this original report group is a waypoint
C*		    identifier, and, if so, store the associated
C*		    latitude and longitude values.
C
		    CALL AF_AWPT  ( field, lenf, cicli, ierwpt )
		END IF
		irfn = irfn + 1
	    END DO
	END IF
C 
C*	Locate, decode, and store the time (i.e. hour/minute) data
C*	from within the 7 "like-type" groups...
C 
	IF  ( ( ielat .ne. IMISSD ) .or. ( ielon .ne. IMISSD ) )  THEN
C 
C*	    following the lat/long data.
C
	    isdx = MAX0 ( ielat, ielon ) + 1
	ELSE IF  ( ierwpt .ne. (-1) )  THEN
C 
C*	    following the waypoint identifier.
C
	    isdx = ienf + 1
	ELSE
C
C*	    starting with "like-type" group number 1.
C 
	    isdx = 1
	END IF
	CALL AF_ATIM  ( isdx, 7, istim, ietim, ieratm )
C
C*	Locate, decode, and store the flight level data from within
C*	the 5 "like-type" groups...
C
	IF  ( ietim .ne. IMISSD ) THEN
C
C*	    following the time data.
C
	    isdx = ietim + 1
	ELSE
C
C*	    starting with "like-type" group number 6.
C
	    isdx = 6
	END IF
	CALL AF_AFLV  ( isdx, 5, isflv, ieflv, ierafl )
C
C*	Locate, decode, and store the temperature data from within
C*	the 10 "like-type" groups...
C
	IF  ( ietim .ne. IMISSD ) THEN
C
C*	    following the time data.
C
	    isdx = ietim + 1
	ELSE
C
C*	    starting with "like-type" group number 6.
C
	    isdx = 6
	END IF
	CALL AF_ATMP  ( isdx, 10, istmp, ietmp, ieratp )
C
C*	Locate, decode, and store the wind data from within
C*	the 10 "like-type" groups...
C
	IF  ( ietim .ne. IMISSD ) THEN
C
C*	    following the time data.
C
	    isdx = ietim + 1
	ELSE
C
C*	    starting with "like-type" group number 6.
C
	    isdx = 6
	END IF
	CALL AF_AWND  ( isdx, 10, iswnd, iewnd, ierawd )
C
C*	Locate, decode, and store the turbulence data from within
C*	the 12 "like-type" groups... 
C
	IF  ( ietim .ne. IMISSD ) THEN
C
C*	    following the time data.
C
	    isdx = ietim + 1
	ELSE
C
C*	    starting with "like-type" group number 6.
C
	    isdx = 6
	END IF
	CALL AF_ATRB  ( isdx, 12, istrb, ietrb, ieratb )
C*
	RETURN
	END
