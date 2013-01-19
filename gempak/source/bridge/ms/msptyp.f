	SUBROUTINE MS_PTYP ( cline, iprms, ifcstm, rdata, iret )
C************************************************************************
C* MS_PTYP                                                              *
C*									*
C* This subroutine converts the character strings for a precipitation   *
C* type forecast (conditional) to real values and stores them in the    *
C* GEMPAK output array.                                                 *
C*                                                                      *
C* MS_PTYP ( CLINE, IPRMS, IFCSTM, RDATA, IRET )                        *
C*									*
C* Input parameters:							*
C*	CLINE(*)	CHAR*		Forecast data line              *
C*	IPRMS		INTEGER		Position of parameter in output *
C*	IFCSTM		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data                   *
C*	  (IFCSTM,MMPARM)					        *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00						*
C* m.gamazaychikov/SAIC 11/03   Replaced references to AVN/MRF with     *
C*                              references to GFS/GFSX                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cline (*)
	REAL		rdata (ifcstm,MMPARM)
C*	
	REAL		pcode (7)
C*	
	DATA		pcode / 0., 1., 3., 2., 4., 5., 6. /
C------------------------------------------------------------------------
	iret = 0
C
C*	Character codes are the letters R, S and Z, singly for GFS MOS
C*	or in any combination for GFSX MOS.
C*	0 = R
C*	1 = S
C*	2 = Z
C*	3 = RS, SR
C*	4 = RZ, ZR
C*	5 = SZ, ZS
C*	6 = RSZ, RZS, SRZ, SZR, ZRS, ZSR
C
	DO ii = 1, ifcstm
	    indxr = MIN ( INDEX ( cline ( ii ), 'R' ), 1 )
	    indxs = MIN ( INDEX ( cline ( ii ), 'S' ), 1 )
	    indxz = MIN ( INDEX ( cline ( ii ), 'Z' ), 1 )
	    idat  = indxz * 4 + indxs * 2 + indxr
	    IF ( ( idat .gt. 0 ) .and. ( idat .lt. 8 ) ) THEN
	        rdata ( ii, iprms ) = pcode ( idat )
	      ELSE
	        rdata ( ii, iprms ) = RMISSD
	    END IF
	END DO
C*
	RETURN
	END
