	SUBROUTINE TA_BFIF  ( iubfma, iret )
C************************************************************************
C* TA_BFIF								*
C*									*
C* This subroutine decodes a report from an input BUFR message,		*
C* converts it into interface output, and then writes the interface	*
C* output into the interface arrays.					* 
C*									*
C* TA_BFIF  ( IUBFMA, IRET )						*
C*									*
C* Input parameters:							*
C*	IUBFMA		INTEGER		Logical unit number of BUFR	*
C*					messages file			*
C*									*
C* Output parameters:							*
C*	CIVALS (IRACRN)	CHAR		Aircraft tail number
C*	RIVALS (IRYEAR)	REAL		Report year			*
C*	RIVALS (IRMNTH)	REAL		Report month			*
C*	RIVALS (IRDAYS)	REAL		Report day			*
C*	RIVALS (IRHOUR)	REAL		Report hour			*
C*	RIVALS (IRMINU)	REAL		Report minute			*
C*	RIVALS (IRSECO)	REAL		Report second                   *
*	RIVALS (IRSLAT)	REAL		latitude    			*
C*	RIVALS (IRSLON)	REAL		longitude 			*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C  C. Caruso Magee/NCEP 08/06   New                                     *
C  C. Caruso Magee/NCEP 09/06   Add ermiss check on tpqc2 to reset to   *
C                               value of 3 if all bits are set.         *
C  C. Caruso Magee/NCEP 10/07   Add pressure.                           *
C* J. Ator/NCEP		10/08	Add ACTP and OBSVR, and adjust PCCF	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'tacmn.cmn'
C*
	PARAMETER	( NTPQC = 7 )
C*
        REAL*8		r8tpqc ( NTPQC )
        real            rtpqc ( NTPQC )
C*
	INCLUDE		'ERMISS.FNC'
C*-----------------------------------------------------------------------
	iret = 0
C
        CALL UT_BFCI  ( iubfma, 'ACRN', civals ( icacrn ), ierbci )
        CALL UT_BFCI  ( iubfma, 'ACTP', civals ( icactp ), ierbci )
        CALL UT_BFCI  ( iubfma, 'OBSVR', civals ( icobsv ), ierbci )
C
	CALL UT_BFRI  ( iubfma, 'YEAR', rivals ( iryear ), ierbri )
	CALL UT_BFRI  ( iubfma, 'MNTH', rivals ( irmnth ), ierbri )
	CALL UT_BFRI  ( iubfma, 'DAYS', rivals ( irdays ), ierbri )
	CALL UT_BFRI  ( iubfma, 'HOUR', rivals ( irhour ), ierbri )
	CALL UT_BFRI  ( iubfma, 'MINU', rivals ( irminu ), ierbri )
	CALL UT_BFRI  ( iubfma, 'SECO', rivals ( irseco ), ierbri )
	CALL UT_BFRI  ( iubfma, 'CLATH', rivals ( irslat ), ierbri )
	CALL UT_BFRI  ( iubfma, 'CLONH', rivals ( irslon ), ierbri )
	CALL UT_BFRI  ( iubfma, 'HMSL', rivals ( irhmsl ), ierbri )
	CALL UT_BFRI  ( iubfma, 'PRLC', rivals ( irprlc ), ierbri )
	CALL UT_BFRI  ( iubfma, 'FLVLST', rivals ( irflvl ), ierbri )
	CALL UT_BFRI  ( iubfma, 'POAF', rivals ( irpoaf ), ierbri )
	CALL UT_BFRI  ( iubfma, 'IALR', rivals ( irialr ), ierbri )
	CALL UT_BFRI  ( iubfma, 'SMMO', rivals ( irsmmo ), ierbri )
	CALL UT_BFRI  ( iubfma, 'TMDBST', rivals ( irtmdb ), ierbri )
	CALL UT_BFRI  ( iubfma, 'WDIR', rivals ( irwdir ), ierbri )
	CALL UT_BFRI  ( iubfma, 'WSPD', rivals ( irwspd ), ierbri )
	CALL UT_BFRI  ( iubfma, 'TRBXST', rivals ( irtrbx ), ierbri )
	CALL UT_BFRI  ( iubfma, 'TOPEDR', rivals ( irtedr ), ierbri )
	CALL UT_BFRI  ( iubfma, 'AFIC', rivals ( irafic ), ierbri )
	CALL UT_BFRI  ( iubfma, 'RAWHU', rivals ( irrehu ), ierbri )
C
C*	The value being reported for PCCF is actually the percent
C*	uncertainty rather than the percent confidence, so compute
C*	the latter from the former.
C
	CALL UT_BFRI  ( iubfma, 'PCCF', rpcf, ierbri )
	IF ( .not. ERMISS ( rpcf ) ) THEN
	    rivals ( irpccf ) = 100. - rpcf
	END IF
C
C*      Store the number of replications for each repeated variable.
C
        rivals ( irntqc ) = 7
C
C*      Decode and store the assoc. field significance data (i.e. the
C*      quality control bits).
C
	CALL UFBREP  ( iubfma, r8tpqc, 1, NTPQC, ierufb, 'TPQC2' )
        DO i = 1, NTPQC
          rtpqc (i)  = UT_BMRI ( r8tpqc (i) )
          IF  ( ERMISS ( rtpqc (i) ) ) THEN
            rivals ( irtpqc (i) ) = 3.0
          ELSE
            rivals ( irtpqc (i) ) = rtpqc (i)
          END IF
        END DO
C*
        RETURN
	END
