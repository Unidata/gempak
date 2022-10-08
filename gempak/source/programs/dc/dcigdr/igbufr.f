	SUBROUTINE IG_BUFR  ( iubfmn, irundt, seqnum, buhd,
     +			      cborg, bulldt, bbb, corn, iret )
C************************************************************************
C* IG_BUFR								*
C*									*
C* This subroutine retrieves data from the interface arrays, converts	*
C* it into BUFR output, and then writes the BUFR output to the BUFR	*
C* output stream.							*
C*									*
C* IG_BUFR  ( IUBFMN, IRUNDT, SEQNUM, BUHD,				*
C*	      CBORG, BULLDT, BBB, CORN, IRET )				*
C*									*
C* Input parameters:							*
C*	IUBFMN		INTEGER		Logical unit number of messages	*
C*					file for BUFR output stream	*
C*	IRUNDT (5)	INTEGER		Run date-time			*
C*					(YYYY, MM, DD, HH, MM)		*
C*	SEQNUM		CHAR*		Bulletin sequence number	*
C*	BUHD		CHAR*		Bulletin header			*
C*	CBORG		CHAR*		Bulletin originator		*
C*	BULLDT		CHAR*		Bulletin date-time		*
C*	BBB		CHAR*		Bulletin BBB indicator		*
C*	CORN		REAL 		Bulletin correction indicator   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 10/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'igcmn.cmn'
C*
	CHARACTER*(*)	seqnum, buhd, cborg, bulldt, bbb
C*
	CHARACTER	bfstyp*8
C*
        REAL*8          r8ary (2), r8ary1 (9), r8ary2 (2,3), 
     +                  r8ary3 (5), r8ary4 (2,2), r8ary5 (3),
     +                  r8ary6 (4) 
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
	bfstyp = 'NC031106'
C
C*	Open a BUFR message for output.
C
	CALL OPENMB  ( iubfmn, bfstyp, ibfdt )
C
C*	Bulletin ID information.
C
	CALL UT_CIBF  ( iubfmn, 'SEQNUM', seqnum, 8, iercbf )
	CALL UT_CIBF  ( iubfmn, 'BUHD', buhd, 8, iercbf )
	CALL UT_CIBF  ( iubfmn, 'BORG', cborg, 8, iercbf )
	CALL UT_CIBF  ( iubfmn, 'BULTIM', bulldt, 8, iercbf )
	CALL UT_CIBF  ( iubfmn, 'BBB', bbb, 8, iercbf )
C
C*	Report date-time.  Write seconds out to SECO ( 0 04 006 ) instead
C*      of SECW ( 0 04 007 ) for possible dupe-checking, but output bufr
C*      table will rescale SECO to scale and bit-width values of 0 04 007.
C
	CALL UT_RIBF  ( iubfmn, 'YEAR', rivals ( iryear ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'MNTH', rivals ( irmnth ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'DAYS', rivals ( irdays ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'HOUR', rivals ( irhour ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'MINU', rivals ( irminu ), ierrbf )
	CALL UT_RIBF  ( iubfmn, 'SECO', rivals ( irsecw ), ierrbf )
C
C*	Satellite identifier.
C
	CALL UT_RIBF  ( iubfmn, 'SAID', rivals ( irsaid ), ierrbf )
C
C*	Software identification.
C
	CALL UT_RIBF  ( iubfmn, 'SWID', rivals ( irswid ), ierrbf )
C
C*	Originating/generating center.
C
	CALL UT_RIBF  ( iubfmn, 'OGCE', rivals ( irogce ), ierrbf )
C
C*	Orbit number.
C
	CALL UT_RIBF  ( iubfmn, 'ORBN', rivals ( irorbn ), ierrbf )
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
C*	Remotely sensed surface type.
C
	CALL UT_RIBF  ( iubfmn, 'RSST', rivals ( irrsst ), ierrbf )
C
C*	Altimeter echo type.               
C
	CALL UT_RIBF  ( iubfmn, 'AETP', rivals ( iraetp ), ierrbf )
C
C*	Land/sea qualifier.                   
C
	CALL UT_RIBF  ( iubfmn, 'LSQL', rivals ( irlsql ), ierrbf )
C
C*	Altimeter state flag.       
C
	CALL UT_RIBF  ( iubfmn, 'ASFL', rivals ( irasfl ), ierrbf )
C
C*	Radiometer state flag.           
C
	CALL UT_RIBF  ( iubfmn, 'RSFL', rivals ( irrsfl ), ierrbf )
C
C*	3-D error estimate of the navigator orbit.
C
	CALL UT_RIBF  ( iubfmn, 'EENO', rivals ( ireeno ), ierrbf )
C
C*	Square of the off-nadir angle.
C
	CALL UT_RIBF  ( iubfmn, 'SONA', rivals ( irsona ), ierrbf )
C
C*	Radiometer water vapor content.
C
	CALL UT_RIBF  ( iubfmn, 'RWVC', rivals ( irrwvc ), ierrbf )
C
C*	Radiometer liquid content.
C
	CALL UT_RIBF  ( iubfmn, 'RLQC', rivals ( irrlqc ), ierrbf )
C
C*	Corrected report indicator.  Corn may have been set to non-zero
C*      in IG_DCOD, so only check here to see if it's still zero and
C*      then set it if BBB indicates corrected bulletin.
C
	IF ( corn .eq. 0.0 ) THEN
	    IF  ( bbb (1:1) .eq. 'C' )  THEN
		corn = 1.0
	    END IF
	END IF
	CALL UT_RIBF  ( iubfmn, 'CORN', corn, ierrbf )
C
C*      Store replicated/multi-level parameters.
C*      First, do sig wave height.
C
        r8ary ( 1 ) = UT_RIBM ( rivals ( irsgw1 ) )
        r8ary ( 2 ) = UT_RIBM ( rivals ( irsgw2 ) )
        CALL UFBREP ( iubfmn, r8ary, 1, 2, ierufb, 'SGWH' )
C
C*      Store type of band.
C
        DO i = 1,2
          r8ary ( i ) = UT_RIBM ( rivals ( irtobd ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary, 1, 2, ierufb, 'TOBD' )
C
C*      Store satellite sensor indicator data.
C
        r8ary ( 1 ) = UT_RIBM ( rivals ( irssi1 ) )
        r8ary ( 2 ) = UT_RIBM ( rivals ( irssi2 ) )
        CALL UFBREP ( iubfmn, r8ary, 1, 2, ierufb, 'SSIN' )
C
C*      Store quality marks.
C
        r8ary1 ( 1 ) = UT_RIBM ( rivals ( ir1tqc ) )
        DO i = 1,2
          r8ary1 ( i*2 )     = UT_RIBM ( rivals ( irqcb1 ( i ) ) )
          r8ary1 ( (i*2)+1 ) = UT_RIBM ( rivals ( irqcb2 ( i ) ) )
        END DO
        r8ary1 ( 6 ) = UT_RIBM ( rivals ( ir2tqc ) )
        DO i = 1,3
          r8ary1 ( i+6 ) = UT_RIBM ( rivals ( irqctm ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary1, 1, 9, ierufb, 'QMRK' )
C
C*      Store mean frequency and brightness temperature.
C*      Convert mean frequency from MHz back to Hz for output.
C
        DO i = 1,3
          r8ary2 ( 1,i ) = UT_RIBM ( PR_HGKM ( PR_HGKM 
     +                              ( rivals ( irmefr ( i ) ) ) ) )
          r8ary2 ( 2,i ) = UT_RIBM ( rivals ( irtmbr (i) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary2, 2, 3, ierufb, 'MEFR TMBR' )
C
C*      Store first order statistics.
C
        r8ary3 ( 1 ) = UT_RIBM ( rivals ( irfost ) )
        DO i = 1,2
          r8ary3 ( i*2 )     = UT_RIBM ( rivals ( irfos1 ( i ) ) )
          r8ary3 ( (i*2)+1 ) = UT_RIBM ( rivals ( irfos2 ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary3, 1, 5, ierufb, 'FOST' )
C
C*      Store station elevation.
C
        r8ary3 ( 1 ) = UT_RIBM ( rivals ( irselv ) )
        DO i = 1,2
          r8ary3 ( i*2 )     = UT_RIBM ( rivals ( irelb1 ( i ) ) )
          r8ary3 ( (i*2)+1 ) = UT_RIBM ( rivals ( irelb2 ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary3, 1, 5, ierufb, 'SELV' )
C
C*      Store 10 meter wind and satellite derived wind computation method.
C
        DO i = 1,2
          r8ary4 ( 1, i ) = UT_RIBM ( rivals ( irswcm ( i ) ) )
          r8ary4 ( 2, i ) = UT_RIBM ( rivals ( irws10 ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary4, 2, 2, ierufb, 'SWCM WS10' )
C
C*      Store number of valid points per second used to derive 
C*      previous parameters.
C
        r8ary5 ( 1 ) = UT_RIBM ( rivals ( irnvpp ) )
        DO i = 1,2
          r8ary5 ( i+1 ) = UT_RIBM ( rivals ( irnvpb ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary5, 1, 3, ierufb, 'NVPP' )
C
C*      Store height increment.                                
C
        r8ary5 ( 1 ) = UT_RIBM ( rivals ( irhinc ) )
        DO i = 1,2
          r8ary5 ( i+1 ) = UT_RIBM ( rivals ( irhinb ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary5, 1, 3, ierufb, 'HINC' )
C
C*      Store backscatter.
C
        DO i = 1,2
          r8ary6 ( (i*2)-1 ) = UT_RIBM ( rivals ( irbks1 ( i ) ) )
          r8ary6 ( i*2 )     = UT_RIBM ( rivals ( irbks2 ( i ) ) )
        END DO
        CALL UFBREP ( iubfmn, r8ary6, 1, 4, ierufb, 'BKST' )
C
	CALL UT_WBFR  ( iubfmn, 'igdr', .false., ierwbf )
C*
	RETURN
	END
