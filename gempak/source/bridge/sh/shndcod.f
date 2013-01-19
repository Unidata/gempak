	SUBROUTINE SHN_DCOD  ( cldt, shefpm, sheftb, bufrtb, gemfil, 
     +			       prmfil, pestr, npe, iadstn, maxtim, 
     +                         nhours, iflag, iret)
C************************************************************************
C* SHN_DCOD								*
C*									*
C* This routine reads SHEF bulletins from the LDM and decodes them into	*
C* BUFR format.								*
C*									*
C* SHN_DCOD ( CLDT, SHEFPM, SHEFTB, BUFRTB, GEMFIL, PRMFIL,             * 
C*            PESTR, NPE, IADSTN, MAXTIM, NHOURS, IFLAG, IRET )  	*
C*									*
C* Input parameters:							*
C*	CLDT		CHAR*		Date-time from command line	*
C*	SHEFPM		CHAR*		SHEFPARM parameter file		*
C*	SHEFTB		CHAR*		SHEF station table		*
C*	BUFRTB		CHAR*		BUFR tables file		*
C*      GEMFIL          CHAR*           Output file name template       *
C*      PRMFIL          CHAR*           Parameter packing table         *
C*	PESTR		CHAR*		String of PE codes for which to	*
C*	NPE		INTEGER		Number of PE codes in PESTR	*
C*      IADSTN          INTEGER         Number of additional stations   *
C*      MAXTIM          INTEGER         Number of times allowed         *
C*	NHOURS		INTEGER		Max # of hours before run time	*
C*					for creating BUFR output	*
C*      IFLAG           INTEGER         FLAG FOR BUFR or GEMPAK output  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					0 = normal return		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C* J. Ator/NCEP		10/06	Added PE codes to argument list		*
C* V. K. Kumar/NCEP     04/07   Added a call to FL_TINQ to read         *
C*                              "shef.prm" and "bufrtab.xxx" from       *
C*                              the default or local directories        * 
C*                              $GEMTBL/pack $GEMTBL/bufrlib resply     * 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
	INCLUDE		'shncmn_pe.cmn'
C*
	CHARACTER	rundt*12, sysdt*12, cdmyfl*15, peymd*6,
     +			pefil*132, pefilst*3, dattim*15,
     +                  filnam*132
C*
C*
        CHARACTER       fname*132
        LOGICAL         exist
C
        CHARACTER       parms ( MMPARM )*4, dprms ( MMPARM )*4
C*
        PARAMETER       ( NUMP = 26 , NUMPRM = NUMP )
        PARAMETER       ( NUMEXT = MMPARM - NUMP )
C*
C*      Maintain the order of the following parameter list.  Add new
C*      parameters at the end.
C
        DATA            dprms / 'PR24', 'SHPT', 'SHHG', 'SF24',
     +                          'SNOW', 'WEQS', 'TMPF', 'TDXF',
     +                          'TDNF', 'TMWF', 'DWPF', 'TWAF',
     +                          'RELH', 'DRCT', 'PKWD', 'SMPH',
     +                          'GMPH', 'WSGU', 'WDIR', 'WSPD',
     +                          'PRES', 'PMSL', 'PWWM', 'WWMO',
     +                          'VSBY', 'HR24',
     +                           NUMEXT * ' ' /

C*
	CHARACTER*(*)	cldt, shefpm, sheftb, bufrtb, pestr, 
     +                  gemfil, prmfil
C*
	INTEGER		iundmy(3)
C*
	LOGICAL		bullok, exists, addstn, cirflg
C*
	DATA		itype / 1 /
C------------------------------------------------------------------------
	iret = 0
C
C* FLAG to write either a BUFR output or GEMPAK output from iflag
C  to be saved in the interface array common
C
        iflagw = iflag
C*
        dattim = cldt
C
C*      Initalize the GEMPAK parameter array and establish the dprms array
C*      done at the top
C
        DO i = 1, NUMP
           cprms ( i ) = dprms ( i )
        END DO
C
C*      Initialize open file lists. Set the max number of open files.
C*      Set the type of output file and file source.
C
        maxfil = 1
        iftype = 1
        CALL DC_FINT ( maxfil, iftype, prmfil, ier )
        addstn = .true.
        cirflg = .false.
C
C*	Open the SHEFPARM parameter file .
C
        CALL FL_TINQ ( shefpm, 'pack', exist, fname, iertnq )
        IF ( iertnq .ne. 0 ) THEN
            CALL DC_WLOG ( 0, 'FL', iertnq, shefpm, ierwlg )
            RETURN
        END IF
C
	CALL FL_SOPN ( fname, iunshp, ierspn )
	IF ( ierspn .ne. 0 ) THEN
	    CALL DC_WLOG ( 0, 'FL', ierspn, shefpm, ierwlg )
	    RETURN
	END IF
C
C*	Open and read the SHEF station table.
C
	CALL SHN_STOR ( sheftb, iersto )
	IF ( iersto .ne. 0 ) THEN
	    RETURN
	END IF
C
C*	Initialize COMMON / PECODES /
C
	IF ( npe .gt. MXPECOD ) THEN
	    WRITE ( UNIT = logmsg, FMT = '( A, I2, 2A, I2, A )' )
     +		 'There were ', npe, ' PE codes on the command line,',
     +		 ' but only the first ', MXPECOD, ' were processed.'
	    CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg )
	    npecod = MXPECOD
	ELSE
	    npecod = npe
	END IF
C
C*	Were there any PE codes on the command line?
C
	IF ( npecod .gt. 0 ) THEN
C
C*	    Copy the PE codes into COMMON / PECODES /
C
	    DO ii = 1, npecod
		pecod (ii) = pestr((2*ii-1):(2*ii))
	    END DO
C
C*	    Open the ASCII output file for the PE codes.
C
	    CALL CSS_GTIM ( itype, sysdt, iergtm )
	    IF ( iergtm .ne. 0 )  THEN
		CALL DC_WLOG ( 0, 'SS', iergtm, ' ', ierwlg )
		RETURN
	    END IF
	    peymd = sysdt(1:6)
C
C	    CALL SS_ENVR ( '$PEDIR', pedir, ierenv )
C	    CALL ST_LSTR ( pedir, lped, ierstr )
C	    IF ( ( ierenv .ne. 0 ) .or. ( lped .eq. 0 ) ) THEN
C		logmsg = 'Environment variable $PEDIR is undefined'
C		CALL DC_WLOG ( 0, 'DC', 2, logmsg, ierwlg )
C		RETURN
C	    END IF
C	    pefil = pedir(1:lped) // '20' // peymd // '.pe'
	    pefil = '20' // peymd // '.pe'
C
	    CALL FL_GLUN ( iunpef, iergln )
	    IF ( iergln .ne. 0 ) THEN
		CALL DC_WLOG ( 0, 'FL', iergln, ' ', ierwlg )
		RETURN
	    END IF
	    INQUIRE ( FILE = pefil, EXIST = exists )
	    IF ( exists ) THEN
		pefilst = 'OLD'
	    ELSE
		pefilst = 'NEW'
	    END IF 
	    OPEN ( UNIT = iunpef, FILE = pefil, STATUS = pefilst )
	END IF
C
C*	Open dummy files for use by the OH SHEFLIB parsing software.
C
C	cdmyfl = '.dummy/dcshef_'
	cdmyfl = '.dcshef_'
	DO ii = 1, 3
	    WRITE ( UNIT = cdmyfl(15:15), FMT = '(I1.1)')  ii
	    CALL FL_SWOP ( cdmyfl, iundmy (ii), iergln )
	    IF ( iergln .ne. 0 ) THEN
		CALL DC_WLOG  ( 0, 'FL', iergln, cdmyfl, ierwlg )
		RETURN
	    END IF
	END DO
C
C*	Initialize some variables for use by subroutine SHN_DFHR.
C
        CALL CLO_INIT ( iercit )
        IF ( iercit .ne. 0 ) THEN
            CALL UT_EMSG ( 2, 'CLO_INIT', iercit )
	    RETURN
        END IF
        CALL SHN_CLIN
C
C*	Open the BUFR tables file.
C
        CALL FL_TINQ ( bufrtb, 'bufrlib', exist, fname, iertnq )
        IF ( iertnq .ne. 0 ) THEN
            CALL DC_WLOG ( 0, 'FL', iertnq, bufrtb, ierwlg )
            RETURN
        END IF

	CALL FL_SOPN  ( fname, iunbft, ierspn )
	IF  ( ierspn .ne. 0 )  THEN
	    CALL DC_WLOG  ( 0, 'FL', ierspn, bufrtb, ierwlg )
	    RETURN
	END IF
C
C*	Open the BUFR output file.
C
	CALL FL_GLUN ( iunbfo, iergln )
	IF ( iergln .ne. 0 ) THEN
	    CALL DC_WLOG ( 0, 'FL', iergln, ' ', ierwlg )
	    RETURN
	END IF
C
C*      Make a file name from the template and the time.
C       Open the file as part of the open file list.
C
         lunf = iungem
C
         iflsrc = 2 + MFTEXT
         CALL FL_MNAM ( dattim, gemfil, filnam, ier )

         CALL DC_FCYL ( filnam, iflsrc, sheftb, iadstn,
     +                  maxtim, iungem, nparm, parms, ierr )
C
C*       Check that the file was opened properly.
C
         IF ( ierr .ne. 0 ) THEN
C
C*          If not, write an error to the decoder
C*          log file.
C
            CALL DC_WLOG ( 0, 'DC', ierr, filnam, iret )
          END IF
C
C*	Connect the BUFR tables and output files to the BUFR interface.
C
	CALL OPENBF ( iunbfo, 'NUL', iunbft )
C
C*	Close the BUFR tables file.
C
	CALL FL_CLOS ( iunbft, iercls )
	IF ( iercls .ne. 0 ) THEN
	    CALL DC_WLOG  ( 0, 'FL', iercls, ' ', ierwlg )
	END IF
C
	DO WHILE ( .true. )
C
C*	    Get a new bulletin from the input pipe.
C
	    CALL DC_GBUL ( bull, lenb, ifdtyp, iergbl )
	    IF ( iergbl .ne. 0 ) THEN
C
C*		A time-out occurred while waiting for a new bulletin
C*		on the input pipe.  Shut down the decoder and exit.
C
		CALL DC_WLOG ( 0, 'DC', iergbl, ' ', ierwlg )
		CLOSE ( iunpef )
		CALL CLOSBF ( iunbfo )
		CALL FL_CLAL ( iercal )
		RETURN
	    END IF
C
C*	    Decode the header information from this bulletin.
C
	    IF ( ifdtyp .eq. 0 ) THEN
C
C*		Decode WMO products.
C
		CALL DC_GHDR ( bull, lenb, seqnum, buhd, cborg,
     +			       bulldt, bbb, ibptr, ierghd )
		IF ( ierghd .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierghd, ' ', ierwlg )
		    bullok = .false.
		ELSE
C
C*		    Initialize some values for processing of this
C*		    bulletin by the OH SHEFLIB parsing software.
C
		    CALL SHN_IFIV ( ierifi )
		    ibptr = 1
		    bullok = .true.
		END IF
	    ELSE
C
C*		Do not decode AFOS products.
C
		bullok = .false.
	    END IF
	    IF ( bullok ) THEN
C
C*		Get the system time.
C
		CALL CSS_GTIM ( itype, sysdt, iergtm )
		IF  ( iergtm .ne. 0 )  THEN
		    CALL DC_WLOG ( 2, 'SS', iergtm, ' ', ierwlg )
		    bullok = .false.
		END IF
	    END IF
	    IF ( bullok ) THEN
		IF ( ( npecod .gt. 0 ) .and.
     +			( sysdt(1:6) .ne. peymd ) ) THEN
C
C*		    Start a new PE output file.
C
		    CLOSE ( iunpef )
		    peymd = sysdt(1:6)
		    pefil( lped+3 : lped+8 ) = peymd
	    	    OPEN ( UNIT = iunpef, FILE = pefil, STATUS = 'NEW' )
		END IF
C
C*		If a date-time was entered on the command line, then
C*		use it as the run date-time.  Otherwise, use the
C*		system time as the run date-time.
C
		IF ( cldt .eq. 'SYSTEM' ) THEN
		    rundt = sysdt
		ELSE
		    CALL TI_STAN ( cldt, sysdt, rundt, ierstn )
		    IF ( ierstn .ne. 0 ) THEN
			CALL DC_WLOG ( 2, 'TI', ierstn, ' ', ierwlg )
			bullok = .false.
		    END IF
		END IF
	    END IF
	    IF ( bullok ) THEN
C
C*		Convert the run date-time to integer.
C
		CALL TI_CTOI ( rundt, irundt, iercto )
		IF ( iercto .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'TI', iercto, ' ', ierwlg )
		    bullok = .false.
		END IF
	    END IF
	    IF ( bullok ) THEN
C
C*		Start an entry for this bulletin in the decoder log.
C
		logmsg = '####################' //
     +			 '####################' //
     +			 '####################'
		CALL DC_WLOG ( 2, 'DC', 2, logmsg, ierwlg )
C
C*		Call the OH SHEFLIB parsing software.
C
		CALL SHDRIV ( iundmy (1), iundmy (2), iunshp,
     +			      iundmy (3), iundmy (3) )
C
C*		Make sure that any remaining data in the interface
C*		arrays has been converted into BUFR and written out
C*		before going back to DC_GBUL and waiting for a new
C*		bulletin on the input pipe.
C
		IF ( nimn .gt. 8 ) THEN
		    CALL SHN_IFPT ( 3, ierifp )
                    IF ( iflagw .eq. 1 ) THEN
		       CALL SHN_BUFR ( ierbfr )
                    ELSE
                       CALL SHN_GEMP ( iergem )
                    END IF
		END IF
                IF ( iflagw .eq. 1 ) THEN
		   CALL UT_WBFR  ( iunbfo, 'shef', .true., ierwbf )
                END IF
	    END IF
C
	END DO
C*
	RETURN
	END
