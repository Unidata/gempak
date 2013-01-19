	SUBROUTINE OANDSP  ( gdfile, gltln, iextnd, kex, key, deltan,
     +			     datfil, area, parms, nparms, rlevel,
     +			     nlevel, vcoord, stnprm, nstnpm, time, 
     +			     gamma, search, npass, gsfil,
     +			     gsflag, gsdttm, extrap, qcntl, oabnd,
     +                       iret )
C************************************************************************
C* OANDSP								*
C*									*
C* This subroutine displays the variables to be used in OABND.		*
C*									*
C* OANDSP  ( GDFILE, GLTLN, IEXTND, KEX, KEY, DELTAN, DATFIL, AREA,	*
C*           PARMS, NPARMS, RLEVEL, NLEVEL, VCOORD, STNPRM, NSTNPM,	*
C*           TIME, GAMMA, SEARCH, NPASS, GSFIL, GSFLAG,        		*
C*           GSDTTM, EXTRAP, QCNTL, OABND, IRET )			*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	GLTLN  (4)	REAL		Grid area bounds		*
C*	IEXTND (4)	INTEGER		First pass extension		*
C*	KEX		INTEGER		# of x points in extend area	*
C*	KEY		INTEGER		# of y points in extend area	*
C*	DELTAN		REAL		Station spacing			*
C*	DATFIL		CHAR*		Input data file			*
C*	AREA		CHAR*		Area				*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	RLEVEL (NLEVEL)	REAL		Levels				*
C*	NLEVEL		INTEGER		Number of levels		*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*	STNPRM (NSTNPM)	CHAR*		Station parameters		*
C*	NSTNPM		INTEGER		Number of station parameters	*
C*	TIME 		CHAR*		Analysis time			*
C*	GAMMA		REAL		Gamma				*
C*	SEARCH		REAL		Search				*
C*	NPASS		INTEGER		Number of passes		*
C*      GSFIL           CHAR*           Guess file name			*
C*      GSFLAG          LOGICAL         Flag for guess			*
C*      GSDTTM          CHAR*           Date time for guess		*
C*	EXTRAP		LOGICAL		Extrapolation flag		*
C*	QCNTL (*)	REAL		QC threshold			*
C*	OABND(*)	CHAR*		Boundaries for blocking		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = user typed EXIT		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	10/88	Documentation				*
C* K. Brill/GSC          4/90   Guess file related output		*
C* S. Schotz/GSC	 5/90	Get respnd flag locally from IP_RESP	*
C* K. Brill/NMC          8/90   Display single time			*
C* K. Brill/NMC         10/90   Eliminate blank lines from output	*
C* K. Brill/NMC		06/91	Added EXTRAP				*
C* T. Lee/GSC		 3/99	Added QCNTL				*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* J. Wu/SAIC		 4/05	Added OABND				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		gltln (4), qcntl (*)
	LOGICAL		gsflag, extrap, respnd
	CHARACTER*(*)	gdfile, gsfil, parms (*), time, datfil,
     +			area, gsdttm, vcoord, stnprm (*), oabnd
	INTEGER		iextnd (4)
	REAL		rlevel (*)
C*
	INTEGER		ilevel (LLMXLV)
C------------------------------------------------------------------------
	iret = 0
C
C*	Compute the number of points in the grid area.
C
	kx = kex - iextnd (1) - iextnd (3)
	ky = key - iextnd (2) - iextnd (4)
C
C*	Write information to users terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( / ' OABSND PARAMETERS: ' / )
	CALL ST_LSTR  ( gdfile, lengd, ier )
	WRITE  ( 6, 1001 ) gdfile ( : lengd )
1001	FORMAT ( ' Output grid file:        ', A )
	WRITE  ( 6, 1002, IOSTAT = iostat )   kx, ky,
     +			   ( gltln (i), i = 1, 4 ), 
     +			   ( iextnd (i), i = 1, 4 )
1002	FORMAT ( ' Number of x, y points:   ', I3, 3X, I3 /
     +           ' Grid bnds: ', 4F9.2 ,
     +           ' Grid xtnd: ', 4I4 )
	WRITE  ( 6, 1003, IOSTAT = iostat ) deltan
1003	FORMAT ( ' Station spacing:         ', F6.2 )
C*
	IF ( gsflag ) THEN
	  CALL ST_LSTR ( gsfil, lengd, ier )
	  WRITE ( 6, 1201 ) gsfil ( : lengd )
1201      FORMAT ( ' Guess grid file:  ', A )
	  CALL ST_LSTR ( gsdttm, lengd, ier )
	  IF ( lengd .gt. 1 ) THEN
	    WRITE ( 6, 1203 ) gsdttm ( : lengd )
1203        FORMAT ( ' Guess time:  ', A )
	  END IF
	ELSE
          WRITE ( 6, 1205 )
1205	  FORMAT ( ' No first guess file is requested. ')
	END IF
C*
	CALL ST_LSTR  ( datfil, lendt, ier )
	CALL ST_LSTR  ( area, lenar, ier )
	WRITE  ( 6, 1010 ) datfil ( : lendt ), area ( : lenar )
1010	FORMAT ( ' Sounding file:           ', A /
     +           ' Data area:               ', A )
C*
	WRITE  ( 6, 1020 ) ( parms (i), i = 1, nparms )
1020	FORMAT ( ' Level parameters:      ', ( 9 ( 2X, A ) ) )
	WRITE  ( 6, 1021 ) ( qcntl (i), i = 1, nparms )
1021	FORMAT ( ' QC threshold:          ', ( 9 ( F6.1 ) ) )
	DO  i = 1, nlevel
	    ilevel (i) = rlevel (i)
	END DO
	WRITE  ( 6, 1022 ) ( ilevel (i), i = 1, nlevel )
1022	FORMAT ( ' Levels:              ', ( 8 ( 1X, I7 ) ) )
	WRITE  ( 6,1023 )  vcoord
1023	FORMAT ( ' Vertical coordinate:     ', A  )
	WRITE  ( 6, 1024 ) ( stnprm (i), i = 1, nstnpm )
1024	FORMAT ( ' Station parameters:    ', ( 9 ( 2X, A ) ) )
	WRITE  ( 6, 1025 )  time
1025	FORMAT ( ' Time:                   ', A )
	WRITE  ( 6, 1030 ) gamma, search, npass
1030	FORMAT ( ' GAMMA: ', F4.2, '   SEARCH:  ', F5.2, '  NPASS:  ',
     +           I1 )
	WRITE ( 6, 1031 ) oabnd
1031    FORMAT ( ' Valid blocking bounds:  ', A )
C
C*	Write out extrapolation condition.
C
	IF ( extrap ) THEN
	    WRITE ( 6, 1040 )
1040	    FORMAT ( ' Data extrapolation may occur. ' )
	ELSE
	    WRITE ( 6, 1041 )
1041	    FORMAT ( ' No data extrapolation will occur. ' )
	END IF
C
C*	Prompt user.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
