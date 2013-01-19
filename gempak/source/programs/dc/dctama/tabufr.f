	SUBROUTINE TA_BUFR  ( iubfmn, irundt, corn, iret )
C************************************************************************
C* TA_BUFR								*
C*									*
C* This subroutine retrieves data from the interface arrays, converts	*
C* it into BUFR output, and then writes the BUFR output to the BUFR	*
C* output stream.							*
C*									*
C* TA_BUFR  ( IUBFMN, IRUNDT, CORN, IRET )				*
C*									*
C* Input parameters:							*
C*	IUBFMN		INTEGER		Logical unit number of messages	*
C*					file for BUFR output stream	*
C*	IRUNDT (5)	INTEGER		Run date-time			*
C*					(YYYY, MM, DD, HH, MM)		*
C*	CORN		REAL 		Bulletin correction indicator   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 08/06						*
C* C. Caruso Magee/NCEP 08/07  Add restricted data mnemonics RSRD and   *
C*                             EXPRSRD to output.                       *
C* C. Caruso Magee/NCEP 10/07  Add pressure to output.                  *
C* J. Ator/NCEP		10/08	Add ACTP and OBSVR			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C???	INCLUDE		'BUFR.CMN'
	INCLUDE		'tacmn.cmn'
C*
	CHARACTER	bfstyp*8
C*
        REAL*8          r8ary (7), UT_RIBM, PKFTBV 
        REAL            corn
C*
	INTEGER		irundt (5)
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	iret = 0
C
C*	Set the BUFR message date-time.
C
	year = rivals ( iryear )
	rmth = rivals ( irmnth )
	days = rivals ( irdays )
	hour = rivals ( irhour )
	IF  ( ( ERMISS ( year ) ) .or. ( ERMISS ( rmth ) ) .or.
     +	      ( ERMISS ( days ) ) .or. ( ERMISS ( hour ) )  )  THEN
	    RETURN
	END IF
	ibfdt = ( INT ( year ) * 1000000 ) + ( INT ( rmth ) * 10000 )  +
     +		( INT ( days ) * 100 ) + INT ( hour )
C
C*	Set the BUFR message subtype.
C
	bfstyp = 'NC004010'
C
C*	Open a BUFR message for output.
C
	CALL OPENMB  ( iubfmn, bfstyp, ibfdt )
C
C*	Report date-time.                                                
C
	CALL UT_RIBF  ( iubfmn, 'YEAR', rivals ( iryear ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'MNTH', rivals ( irmnth ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'DAYS', rivals ( irdays ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'HOUR', rivals ( irhour ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'MINU', rivals ( irminu ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'SECO', rivals ( irseco ), ierrbf )
C
C*      Restrictions on redistribution.
C
        rsrd = PKFTBV (9,2)
        CALL UT_RIBF  ( iubfmn, 'RSRD', rsrd, ierrbf )
        CALL UT_RIBF  ( iubfmn, 'EXPRSRD', 48., ierrbf )
C
C*	Aircraft tail number.
C
	CALL UT_CIBF  ( iubfmn, 'ACRN', civals ( icacrn ), 8, iercbf )
C
C*	Aircraft type.
C
	CALL UT_CIBF  ( iubfmn, 'ACTP', civals ( icactp ), 8, iercbf )
C
C*	Observer identification.
C
	CALL UT_CIBF  ( iubfmn, 'OBSVR', civals ( icobsv ), 8, iercbf )
C
C*	Latitude and longitude.
C
	CALL UT_RIBF  ( iubfmn, 'CLATH', rivals ( irslat ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'CLONH', rivals ( irslon ), ierrbf )
C
C*	Receipt date-time.
C
	CALL UT_RIBF  ( iubfmn, 'RCYR', FLOAT ( irundt (1) ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'RCMO', FLOAT ( irundt (2) ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'RCDY', FLOAT ( irundt (3) ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'RCHR', FLOAT ( irundt (4) ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'RCMI', FLOAT ( irundt (5) ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'RCTS', 0., ierrbf )
C
C*	Height or Altitude. 
C
	CALL UT_RIBF  ( iubfmn, 'HMSL', rivals ( irhmsl ), ierrbf )
C
C*	Pressure.                     
C
	CALL UT_RIBF  ( iubfmn, 'PRLC', rivals ( irprlc ), ierrbf )
C
C*	Flight Level.                      
C
	CALL UT_RIBF  ( iubfmn, 'FLVLST', rivals ( irflvl ), ierrbf )
C
C*	Phase of flight.                      
C
	CALL UT_RIBF  ( iubfmn, 'POAF', rivals ( irpoaf ), ierrbf )
C
C*	Instantaneous altitude rate.
C
	CALL UT_RIBF  ( iubfmn, 'IALR', rivals ( irialr ), ierrbf )
C
C*	Speed of motion of moving observing platform.
C
	CALL UT_RIBF  ( iubfmn, 'SMMO', rivals ( irsmmo ), ierrbf )
C
C*	Temperature/dry-bulb temperature.
C
	CALL UT_RIBF  ( iubfmn, 'TMDBST', rivals ( irtmdb ), ierrbf )
C
C*	Wind direction.
C
	CALL UT_RIBF  ( iubfmn, 'WDIR', rivals ( irwdir ), ierrbf )
C
C*	Wind speed.                     
C
	CALL UT_RIBF  ( iubfmn, 'WSPD', rivals ( irwspd ), ierrbf )
C
C*	Turbulence index.          
C
	CALL UT_RIBF  ( iubfmn, 'TRBXST', rivals ( irtrbx ), ierrbf )
C
C*	Time of occurrence of peak eddy dissipation rate.
C
	CALL UT_RIBF  ( iubfmn, 'TOPEDR', rivals ( irtedr ), ierrbf )
C
C*	Airframe icing.                                      
C
	CALL UT_RIBF  ( iubfmn, 'AFIC', rivals ( irafic ), ierrbf )
C
C*	Percent confidence.                              
C
	CALL UT_RIBF  ( iubfmn, 'PCCF', rivals ( irpccf ), ierrbf )
C
C*	Raw relative humidity.                           
C
	CALL UT_RIBF  ( iubfmn, 'RAWHU', rivals ( irrehu ), ierrbf )
C
C*	Corrected report indicator.  Corn may have been set to non-zero
C*      in TA_DCOD, so only check here to see if it's still zero and
C*      then set it if BBB indicates corrected bulletin.
C*      Comment out until we receive these in bulletin form!
C
	IF ( corn .eq. 0.0 ) THEN
C           IF  ( bbb (1:1) .eq. 'C' )  THEN
C             corn = 1.0
C           END IF
	END IF
	CALL UT_RIBF  ( iubfmn, 'CORN', corn, ierrbf )
C
C*      Store replicated/multi-level parameters.
C*      Store quality marks.
C
        DO i = 1,7
          r8ary ( i ) = UT_RIBM ( rivals ( irtpqc ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary, 1, 7, ierufb, 'QMRKH' )
C
	CALL UT_WBFR  ( iubfmn, 'tamdar2', .false., ierwbf )
C*
	RETURN
	END
