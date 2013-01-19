	SUBROUTINE IG_BFIF  ( iubfma, cborg, iret )
C************************************************************************
C* IG_BFIF								*
C*									*
C* This subroutine decodes a report from an input BUFR message,		*
C* converts it into interface output, and then writes the interface	*
C* output into the interface arrays.					*
C*									*
C* IG_BFIF  ( IUBFMA, CBORG, IRET )					*
C*									*
C* Input parameters:							*
C*	IUBFMA		INTEGER		Logical unit number of BUFR	*
C*					messages file			*
C*	CBORG		CHAR*		Bulletin originator		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRYEAR)	REAL		Report year			*
C*	RIVALS (IRMNTH)	REAL		Report month			*
C*	RIVALS (IRDAYS)	REAL		Report day			*
C*	RIVALS (IRHOUR)	REAL		Report hour			*
C*	RIVALS (IRMINU)	REAL		Report minute			*
C*	RIVALS (IRSECW)	REAL		Report second microsec accuracy *
*	RIVALS (IRSLAT)	REAL		latitude    			*
C*	RIVALS (IRSLON)	REAL		longitudey			*
C*	RIVALS (IRSAID)	REAL		satellite id			*
C*	RIVALS (IRSWID)	REAL		software id                     * 
C*	RIVALS (IROGCE)	REAL		originating/generating center   *
C*	RIVALS (IRSSI1)	REAL		1st satellite sensor indicator  *
C*	RIVALS (IRSSI2)	REAL		2nd satellite sensor indicator  *
C*	RIVALS (IRORBN)	REAL		orbit number			*
C*      RIVALS (IRSELV) REAL            station elevation               *
C*	RIVALS (IRHINC)	REAL		height increment                *
C*	RIVALS (IRRSST)	REAL		remotely sensed surface type    *
C*	RIVALS (IRAETP)	REAL		altimeter echo type             *
C*	RIVALS (IRLSQL)	REAL		land/sea qualifier              *
C*	RIVALS (IRASFL)	REAL		altimeter state flag            *
C*	RIVALS (IRRSFL)	REAL		radiometer state flag           *
C*	RIVALS (IREENO)	REAL		3-d error estimate of the       *
C*	                                navigator orbit                 *
C*	RIVALS (IR1TQC)	REAL		1st QC holder (refers to        *
C*	                                sig wave height 1 which follows)*
C*	RIVALS (IRSGW1)	REAL		sig wave height                 *
C*	RIVALS (IRFOST)	REAL		1st order stats - code table    *
C*	                                entry = 10 indicates next parm  *
C*	                                is standard dev. of preceding   *
C*	                                parameter.                      *
C*	RIVALS (IRSGW2)	REAL		sig wave height (std dev)
C*	RIVALS (IRNVPP)	REAL		No. valid pts. per sec used to  *
C*	                                derive previous parameter       *
C*	RIVALS (IRNBKS)	REAL		number of backscatter seq levels*
C*	RIVALS (IRTOBD)	REAL		Type of band             	*
C*	RIVALS (IRQCB1)	REAL		1st backscatter qc holder       *
C*	                                (refers to next parameter)      *
C*	RIVALS (IRBKS1)	REAL		backscatter     		*
C*	RIVALS (IRFOS1)	REAL		1st order stats - code table    *
C*	                                entry = 10 indicates next parm  *
C*	                                is standard dev. of preceding   *
C*	                                parameter.                      *
C*	RIVALS (IRBKS2)	REAL		backscatter std deviation       *
C*	RIVALS (IRQCB2)	REAL		2nd backscatter qc holder       *
C*	                                (refers to next parameter)      *
C*	RIVALS (IRELB1)	REAL		backscatter stn elevation       *
C*	RIVALS (IRHINB)	REAL		backscatter height inc.         *
C*	RIVALS (IRFOS2)	REAL		1st order stats - code table    *
C*	                                entry = 10 indicates next parm  *
C*	                                is standard dev. of preceding   *
C*	                                parameter.                      *
C*	RIVALS (IRELB2)	REAL		backscatter stn elev std dev.   *
C*	RIVALS (IRNVPB)	REAL		No. valid pts. per sec used to  *
C*	                                derive previous parameter       *
C*	RIVALS (IR2TQC)	REAL		2nd QC holder (refers to        *
C*	                                square of off-nadir angle)      *
C*	RIVALS (IRSONA)	REAL		square of off-nadif angle       *
C*	RIVALS (IRNMEF)	REAL		number of mean freq seq levels  *
C*	RIVALS (IRMEFR)	REAL		mean frequency                  *
C*	RIVALS (IRQCTM)	REAL		Brightness temperature qc holder* 
C*	RIVALS (IRTMBR)	REAL		Brightness temperature          * 
C*	RIVALS (IRNSWW)	REAL		number of 10 meter wind levels  *
C*	RIVALS (IRSWCM)	REAL		satellite derived wind comp meth* 
C*	RIVALS (IRWS10)	REAL		10 meter wind speed             * 
C*	RIVALS (IRRWVC)	REAL		radiometer water vapor content  * 
C*	RIVALS (IRRLQC)	REAL		radiometer liquid content       * 
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C  C. Caruso Magee/NCEP 10/05   New for IGDR wind/wave                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'igcmn.cmn'
C*
	CHARACTER*(*)	cborg
C*
	PARAMETER	( NSSIN = 2 )
	PARAMETER	( NSELV = 5 )
	PARAMETER	( NHINC = 3 )
	PARAMETER	( NTPQC = 9 )
	PARAMETER	( NSGWH = 2 )
	PARAMETER	( NFOST = 5 )
	PARAMETER	( NNVPP = 3 )
	PARAMETER	( NTOBD = 2 )
	PARAMETER	( NBKST = 4 )
	PARAMETER	( NMEFR = 3 )
	PARAMETER	( NTMBR = 3 )
	PARAMETER	( NSWCM = 2 )
	PARAMETER	( NWS10 = 2 )
C*
        REAL*8		r8ssin ( NSSIN )
        REAL*8		r8selv ( NSELV )
        REAL*8		r8hinc ( NHINC )
        REAL*8		r8tpqc ( NTPQC )
        REAL*8		r8sgwh ( NSGWH )
        REAL*8		r8fost ( NFOST )
        REAL*8		r8nvpp ( NNVPP )
        REAL*8		r8tobd ( NTOBD )
        REAL*8		r8bkst ( NBKST )
        REAL*8		r8mefr ( NMEFR )
        REAL*8		r8tmbr ( NTMBR )
        REAL*8		r8swcm ( NSWCM )
        REAL*8		r8ws10 ( NWS10 )
C*-----------------------------------------------------------------------
	iret = 0
C
	CALL UT_BFRI  ( iubfma, 'YEAR', rivals ( iryear ), ierbri )
	CALL UT_BFRI  ( iubfma, 'MNTH', rivals ( irmnth ), ierbri )
	CALL UT_BFRI  ( iubfma, 'DAYS', rivals ( irdays ), ierbri )
	CALL UT_BFRI  ( iubfma, 'HOUR', rivals ( irhour ), ierbri )
	CALL UT_BFRI  ( iubfma, 'MINU', rivals ( irminu ), ierbri )
	CALL UT_BFRI  ( iubfma, 'SECW', rivals ( irsecw ), ierbri )
	CALL UT_BFRI  ( iubfma, 'CLATH', rivals ( irslat ), ierbri )
	CALL UT_BFRI  ( iubfma, 'CLONH', rivals ( irslon ), ierbri )
	CALL UT_BFRI  ( iubfma, 'SAID', rivals ( irsaid ), ierbri )
	CALL UT_BFRI  ( iubfma, 'SWID', rivals ( irswid ), ierbri )
	CALL UT_BFRI  ( iubfma, 'OGCE', rivals ( irogce ), ierbri )
	CALL UT_BFRI  ( iubfma, 'ORBN', rivals ( irorbn ), ierbri )
	CALL UT_BFRI  ( iubfma, 'RSST', rivals ( irrsst ), ierbri )
	CALL UT_BFRI  ( iubfma, 'AETP', rivals ( iraetp ), ierbri )
	CALL UT_BFRI  ( iubfma, 'LSQL', rivals ( irlsql ), ierbri )
	CALL UT_BFRI  ( iubfma, 'ASFL', rivals ( irasfl ), ierbri )
	CALL UT_BFRI  ( iubfma, 'RSFL', rivals ( irrsfl ), ierbri )
	CALL UT_BFRI  ( iubfma, 'EENO', rivals ( ireeno ), ierbri )
	CALL UT_BFRI  ( iubfma, 'SONA', rivals ( irsona ), ierbri )
	CALL UT_BFRI  ( iubfma, 'RWVC', rivals ( irrwvc ), ierbri )
	CALL UT_BFRI  ( iubfma, 'RLQC', rivals ( irrlqc ), ierbri )
C
C*      Store the number of replications for each repeated variable.
C
        rivals ( irnbks ) = 2
        rivals ( irnmef ) = 3
        rivals ( irnsww ) = 2
C
C*      Decode and store the satellite sensor indicator data.
C
	CALL UFBREP  ( iubfma, r8ssin, 1, NSSIN, ierufb, 'SSIN' )
	rivals ( irssi1  ) = UT_BMRI ( r8ssin (1) )
	rivals ( irssi2  ) = UT_BMRI ( r8ssin (2) )
C
C*      Decode and store the station elevation data.
C
	CALL UFBREP  ( iubfma, r8selv, 1, NSELV, ierufb, 'SELV' )
	rivals ( irselv ) = UT_BMRI ( r8selv (1) )
        DO i = 1,2
          rivals ( irelb1 (i) ) = UT_BMRI ( r8selv ( i*2 ) )
	  rivals ( irelb2 (i) ) = UT_BMRI ( r8selv ( (i*2)+1 ) )
        END DO
C
C*      Decode and store the height increment data.
C
	CALL UFBREP  ( iubfma, r8hinc, 1, NHINC, ierufb, 'HINC' )
	rivals ( irhinc ) = UT_BMRI ( r8hinc (1) )
        DO i = 1,2
  	  rivals ( irhinb (i) ) = UT_BMRI ( r8hinc ( i+1 ) )
        END DO
C
C*      Decode and store the assoc. field significance data.
C
	CALL UFBREP  ( iubfma, r8tpqc, 1, NTPQC, ierufb, 'TPQC' )
	rivals ( ir1tqc ) = UT_BMRI ( r8tpqc (1) )
        DO i = 1,2
	  rivals ( irqcb1 (i) ) = UT_BMRI ( r8tpqc ( i*2 ) )
	  rivals ( irqcb2 (i) ) = UT_BMRI ( r8tpqc ( (i*2)+1 ) ) 
        END DO
	rivals ( ir2tqc ) = UT_BMRI ( r8tpqc (6) )
        DO i = 1,3
  	  rivals ( irqctm (i) ) = UT_BMRI ( r8tpqc ( i+6 ) )
        END DO
C
C*      Decode and store the sig wave height data.
C
	CALL UFBREP  ( iubfma, r8sgwh, 1, NSGWH, ierufb, 'SGWH' )
	rivals ( irsgw1 ) = UT_BMRI ( r8sgwh (1) )
	rivals ( irsgw2 ) = UT_BMRI ( r8sgwh (2) )
C
C*      Decode and store the first order statistics data.
C
	CALL UFBREP  ( iubfma, r8fost, 1, NFOST, ierufb, 'FOST' )
	rivals ( irfost ) = UT_BMRI ( r8fost (1) )
        DO i = 1,2
	  rivals ( irfos1 (i) ) = UT_BMRI ( r8fost (i*2) )
	  rivals ( irfos2 (i) ) = UT_BMRI ( r8fost ( (i*2)+1 ) )
        END DO
C
C*      Decode and store the number of valid points per second used
C*      to derive previous parameters data.
C
	CALL UFBREP  ( iubfma, r8nvpp, 1, NNVPP, ierufb, 'NVPP' )
	rivals ( irnvpp ) = UT_BMRI ( r8nvpp (1) )
        DO i = 1,2
	  rivals ( irnvpb (i) ) = UT_BMRI ( r8nvpp ( i+1 ) )
        END DO
C
C*      Decode and store the type of band data.
C
	CALL UFBREP  ( iubfma, r8tobd, 1, NTOBD, ierufb, 'TOBD' )
        DO i = 1,2
	  rivals ( irtobd (i) ) = UT_BMRI ( r8tobd (i) )
        END DO
C
C*      Decode and store the backscatter data.
C
	CALL UFBREP  ( iubfma, r8bkst, 1, NBKST, ierufb, 'BKST' )
        DO i = 1,2
	  rivals ( irbks1 (i) ) = UT_BMRI ( r8bkst ( (i*2)-1 ) )
	  rivals ( irbks2 (i) ) = UT_BMRI ( r8bkst ( i*2 ) )
        END DO
C 
C*      Decode and store the mean frequency data.  Convert to MHz
C*      so we can display value in decoder log.
C
	CALL UFBREP  ( iubfma, r8mefr, 1, NMEFR, ierufb, 'MEFR' )
        DO i = 1,3
          amefr = UT_BMRI ( r8mefr (i) )
	  rivals ( irmefr (i) ) = PR_HGMK ( PR_HGMK ( amefr ) )
        END DO
C
C*      Decode and store the brightness temperature data.
C
	CALL UFBREP  ( iubfma, r8tmbr, 1, NTMBR, ierufb, 'TMBR' )
        DO i = 1,3
	  rivals ( irtmbr (i) ) = UT_BMRI ( r8tmbr (i) )
        END DO
C
C*      Decode and store the satellite-derived wind calculation
C*      method data.
C
	CALL UFBREP  ( iubfma, r8swcm, 1, NSWCM, ierufb, 'SWCM' )
        DO i = 1,2
	  rivals ( irswcm (i) ) = UT_BMRI ( r8swcm (i) )
        END DO
C
C*      Decode and store the 10 meter wind speed data.      
C
	CALL UFBREP  ( iubfma, r8ws10, 1, NWS10, ierufb, 'WS10' )
        DO i = 1,2
	  rivals ( irws10 (i) ) = UT_BMRI ( r8ws10 (i) )
        END DO
C*
        RETURN
	END
