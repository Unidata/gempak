        SUBROUTINE LS_WGEM ( lunf, nparm, parms, cprms, imnem,
     +			     numprm, iret )
C************************************************************************
C* LS_WGEM                                                              *
C*                                                                      *
C* This subroutine retrievs interface-stored data, converts it into     *
C* GEMPAK output, and then writes the GEMPAK output to the GEMPAK       *
C* output file.                                                         *
C* 								        *
C* LS_WGEM ( LUNF, NPARM, PARMS, CPRMS, IMNEM, NUMPRM, IRET )           *
C*								        *
C* Input parameters:                                                    *
C*      LUNF           INTEGER        GEMPAK file unit number           *
C*      NPARM          INTEGER        Number of parms in packing table  *
C*      PARMS (*)      CHAR*          List of parms in packing table    *
C*	CPRMS (*)      CHAR*          GEMPAK parms chosen for output    *
C*      IMNEM (*)      INTEGER        Subscript mappings, data to GEMPAK*
C*      NUMPRM         INTEGER        Count of chosen GEMPAK parameters *
C*								        *
C* Output parameters:						        *
C*      IRET           INTEGER        Return code                       *
C*                                      0 = normal return               *
C*                                      1 = problems                    *
C*                                                                      *
C**                                                                     *
C* Log:							                *
C* R. Hollern/NCEP       6/96                                           *
C* D. Kidwell/NCEP       1/98	Changed interface, cleaned up code      *
C* D. Kidwell/NCEP       2/98	Added snow parameters to output         *
C* D. Kidwell/NCEP       3/98	Added time check for max/min temps,     *
C*                              removed rpttim from calling sequence    *
C* T. Lee/GSC		 2/99	Added calls to surface pressure checks	*
C* D. Kidwell/NCEP      10/99	Converted auto weather to manned equiv. *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)  	parms (*), cprms (*)
	INTEGER		imnem (*)
C*
	REAL		rdata (MMPARM)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
        iret = 0
C
C*	Surface pressure quality checks.
C
	CALL LS_QCTL ( ier )
C
C*      Initialize GEMPAK output array.
C
        DO i = 1, MMPARM
            rdata ( i ) = RMISSD
        END DO
C
C*      Get hours and minutes of report obs time.
C 
        ihhmm = 100 * irptdt ( 4 ) + irptdt ( 5 )
C
        DO  k = 1, numprm
C
C*          Check for the parameter in the list.
C
            CALL ST_FIND  ( cprms ( k ), parms, nparm, ilc, ier )
C
            IF ( ilc .eq. 0 )  THEN
                CALL DC_WLOG  ( 2, 'DCLSFC', -3, cprms(k), ierr )
              ELSE   
C
C*              Retrieve the requested data and store it into the
C*		GEMPAK array.
C
                IF ( imnem ( k ) .ne. 0 ) THEN
                    rdata ( ilc ) = rivals ( imnem ( k ) )
C
C*                  Check for missing wind direction interface value.
C
                    IF ( cprms ( k ) .eq. 'DRCT' ) THEN
                        IF ( rdata ( ilc ) .eq. 99. )
     +                       rdata ( ilc ) = RMISSD
C
C*			Check for time period for max and min     
C*			temperatures, and only save 24 hour periods.
C
		      ELSE IF ( cprms ( k ) .eq. 'TDXC' ) THEN
			IF ( rivals ( irdtv1 ) .le. 23.9 )
     +			  rdata ( ilc ) = RMISSD
		      ELSE IF ( cprms ( k ) .eq. 'TDNC' ) THEN
			IF ( rivals ( irdtv2 ) .le. 23.9 )
     +			  rdata ( ilc ) = RMISSD
C
C*			Check for automated present or past weather,
C*			and convert to manned equivalent.
C
		      ELSE IF ( cprms ( k ) .eq. 'WWMO' ) THEN
			IF ( .not. ERMISS ( rivals ( irwwma ) ) )
     +			  rdata ( ilc ) = PR_WMAO ( rivals ( irwwma ) )
		      ELSE IF ( cprms ( k ) .eq. 'PWWM' ) THEN
			IF ( .not. ERMISS ( rivals ( irpwwa ) ) )
     +			  rdata ( ilc ) = PR_PWAO ( rivals ( irpwwa ) )
C
C*			Check for SNOW and convert cm to inches.
C
		      ELSE IF ( cprms ( k ) .eq. 'SNOW' ) THEN
			IF ( .not. ERMISS ( rdata ( ilc ) ) ) THEN
			    IF ( rdata ( ilc ) .lt. 997 ) THEN
         		        rdata (ilc) = PR_MMIN ( rdata(ilc) *10.)
			      ELSE
				rdata ( ilc ) = RMISSD
			    END IF
			END IF
C
C*			Check for SNEW and convert meters to inches.
C
		      ELSE IF ( cprms ( k ) .eq. 'SNEW' ) THEN
			IF ( .not. ERMISS ( rdata ( ilc ) ) )
     +   		  rdata (ilc) = PR_MMIN ( rdata (ilc) * 1000. )
C
C*                      Check for trace precipitation interface value.
C
                      ELSE IF ( rdata ( ilc ) .eq. -1. ) THEN
                        IF ( ( cprms ( k ) .eq. 'P03M' ) .or.
     +			     ( cprms ( k ) .eq. 'P06M' ) .or.
     +			     ( cprms ( k ) .eq. 'P09M' ) .or.
     +		             ( cprms ( k ) .eq. 'P12M' ) .or.
     +		             ( cprms ( k ) .eq. 'P18M' ) .or.
     +		             ( cprms ( k ) .eq. 'P24M' ) )
     +                       rdata ( ilc ) = 0.
                    END IF
                END IF
            END IF
C
        END DO
C
C*	Write the decoded report data to GEMPAK land surface data file.
C
        CALL SF_WDAT ( lunf, ihhmm, rdata, ier )
C
        IF ( ier .ne. 0 ) THEN
C
C*          Write an error message if there was a file write error.
C
            CALL DC_WLOG( 2, 'SF', ier, ' ', ierr )
            iret = 1
            RETURN
        END IF
C*
	RETURN
	END
