      SUBROUTINE DCDOPR ( gemfile, packfl, prstns, nstat, nhour, 
     +                    tblb, tbld, curtim, iexit)


      INCLUDE	'GEMPRM.PRM'
      INCLUDE  'BRIDGE.PRM'
C*
      CHARACTER*(*)	gemfile, packfl, prstns, tblb, tbld
      CHARACTER*(*)     curtim
      INTEGER		nstat, nhour, iexit
C*
      INTEGER iflsrc, maxfiles
      INTEGER iret, ier, iperr
      CHARACTER   bultin*(DCMXBF)
      INTEGER	lenbul
C------------------------------------------------------------------------
      iexit = 0
C
C*    Try to open the file/create if doesn't exist
C
      iflsrc = MFRAOB
      maxfiles = 2
C
     
      CALL DC_FINT  ( maxfiles, iflsrc, packfl ,ier)

C
C*      Loop until a timeout occurs.
C
      iperr = 0
      DO WHILE  ( iperr .eq. 0 )
         idftyp = 0
	 CALL DC_GBUFR (bultin, DCMXBF, lenbul, iperr)
         IF  ( iperr .eq. 0 )  THEN
            
            CALL DECODE_PR(tblb, tbld, bultin, lenbul, gemfile, 
     +		iflsrc, prstns, nstat, nhour, iret)

            IF (iret.ne.0) THEN
                CALL DC_WLOG ( 0, 'DCDOPR', iret, 'DECODE_PR', ier)
	    ELSE
                nbull = nbull + 1
	    ENDIF
                                     
C
         END IF
      END DO

      RETURN
      END
