	SUBROUTINE WW_DCOD ( bultin, lenbul, itype, wnum, strtim, 
     +            	     stptim, tissue, icorr, icancl, rlat, rlon,
     +			     npt, irepl, rnums, iret )
C************************************************************************
C* WW_DCOD 								*
C*									*
C* This subroutine decodes a WWUS40 watch report.                       *
C*                                                                      *
C* WW_DCOD ( BULTIN, LENBUL, ITYPE, WNUM, STRTIM, STPTIM, TISSUE,       *
C*           ICORR, ICANCL, RLAT, RLON, NPT, IREPL, RNUMS, IRET )       *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WWUS40 bulletin             	*
C*	LENBUL		INTEGER		Length of bulletin              *
C*									*
C* Output parameters:							*
C*	ITYPE 		INTEGER		Watch type                   	*
C*					  0 = tornado			*
C*					  1 = severe thunderstorm       *
C*	WNUM  		CHAR*		Watch number                    *
C*	STRTIM		CHAR*  		Watch start time, GEMPAK format *
C*	STPTIM 		CHAR*		Watch end time, GEMPAK format   *
C*	TISSUE 		CHAR*  		Watch issue time, GEMPAK format *
C*	ICORR 		INTEGER		Correction indicator           	*
C*	ICANCL		INTEGER		Watch cancellation indicator   	*
C*	RLAT (*)	REAL   	 	Latitudes of vertex points     	*
C*	RLON (*)	REAL   		Longitudes of vertex points    	*
C*	NPT		INTEGER		Number of vertex points         *
C*	IREPL 		INTEGER		Number of watches replaced      *
C*	RNUMS    	CHAR*		Replaced watch numbers          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = bad issue time            *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/98	Based on MT_DCOD                        *
C* D. Kidwell/NCEP	 4/99	Cleaned up, corrected issue time        *
C* D. Kidwell/NCEP	 7/99	Added replacement check, call ST_LCUC,  *
C*                              US9 -> US40                             *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, wnum, strtim, stptim, tissue, rnums
	REAL   		rlat (*), rlon (*)
C*
	CHARACTER	watch*8000/CHNULL/, bultim*6, dattmp*12, sysdt*12
	INTEGER  	istarr (5), iotarr (5), itime
C------------------------------------------------------------------------
	iret = -2
C
C*	Get the system time, and make a standard GEMPAK time
C*	from the "current" time.
C
	itime = 1
	CALL CSS_GTIM  ( itime, sysdt, ier )
	dattmp = sysdt
	CALL TI_CTOI ( dattmp, istarr, ier )
C
        iotarr ( 1 ) = IMISSD
        iotarr ( 2 ) = IMISSD
        iotarr ( 3 ) = IMISSD
        iotarr ( 4 ) = IMISSD
        iotarr ( 5 ) = IMISSD
C
	itype        = IMISSD
	wnum	     = ' '
	strtim 	     = ' '
	stptim	     = ' '
        tissue       = ' '
	icorr	     = IMISSD
	icancl	     = IMISSD
	npt          = 0
	irepl        = 0
C
	DO i = 1, 7
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO
C
C*	Remove unprintables and control characters from bulletin and
C*	ensure upper case.
C
	CALL ST_UNPR ( bultin ( :lenbul ), lenbul, watch, lenw, ier )
	CALL ST_LCUC ( watch, watch, ier )
C
C*	Get issue time for watch.
C
	ibpnt = INDEX ( watch ( :lenw ), ' AWW ' )
	IF ( ibpnt .gt. 0 ) THEN
	    IF ( ( ibpnt + 10 ) .le. lenw ) THEN
		bultim = watch ( ibpnt+5:ibpnt+10 )
		CALL ST_INTG ( bultim, issue, iret )
	    END IF
	END IF
C
        IF ( iret .ge. 0 ) THEN
	    irday  = issue / 10000
            irhour = mod ( issue, 10000 ) / 100
            irmin  = mod ( mod ( issue, 10000 ), 100 )
            CALL WW_RTIM ( istarr, irday, irhour, irmin, iotarr, ier )
	    CALL TI_ITOC ( iotarr, tissue, ier )
	    IF ( ier .eq. 0 ) THEN
C
C*	        Decode remainder of report.
C
	        CALL WW_US40 ( watch, lenw, ibpnt, iotarr, itype,
     +	  	               wnum, strtim, stptim, icorr, icancl,
     +	   	               rlat, rlon, npt, irepl, rnums, ier1 )
	    END IF
	END IF
C*
	RETURN
	END
