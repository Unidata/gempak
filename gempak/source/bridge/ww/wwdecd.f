	SUBROUTINE WW_DECD ( bultin, lenbul, istarr, status, mxp, itype,
     +			   wnum, strtim, stptim, tissue, icorr, icancl,
     +			   rlat, rlon, npt, irepl, rwnum, iret )
C************************************************************************
C* WW_DECD 								*
C*									*
C* This subroutine decodes a single WWUS40 or WWUS30 watch report or    *
C* WWUS8 or WOUS20 status report.                                       *
C*                                                                      *
C* WW_DECD ( BULTIN, LENBUL, ISTARR, STATUS, MXP, ITYPE, WNUM, STRTIM,  *
C* 	     STPTIM, TISSUE, ICORR, ICANCL, RLAT, RLON, NPT, IREPL,     *
C*	     RWNUM, IRET )   	       				        *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Watch or status bulletin        *
C*	LENBUL		INTEGER		Length of bulletin              *
C*	ISTARR (5)	INTEGER		System time - YYYY,MM,DD,HH,MM  *
C*	STATUS		LOGICAL		Status rpt flag (WWUS8, WOUS20) *
C*	MXP   		INTEGER		Maximum number of points        *
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
C*	RLAT (*)	REAL   	 	Latitudes of points     	*
C*	RLON (*)	REAL   		Longitudes of points    	*
C*	NPT		INTEGER		Number of points                *
C*	IREPL		INTEGER		Watch replace / 2nd watch flag  *
C*	RWNUM (*)	CHAR*		Replaced watch numbers          *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = axis point not in table   *
C*					 -2 = bad issue time            *
C*					 -3 = status point not in table *
C*					 -4 = no status points decoded  *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/98	Based on MT_DCOD                        *
C* D. Kidwell/NCEP	 4/99	Cleaned up, corrected issue time        *
C* D. Kidwell/NCEP	 5/99	Modified WW_DCOD --> WW_DECD            *
C* D. Kidwell/NCEP	 5/99	Added replacement check, call ST_LCUC   *
C* D. Kidwell/NCEP	 7/99	WWUS9 -> WWUS40, remove replacement type*
C* D. Kidwell/NCEP	 8/99	Added status report (WWUS8) processing  *
C* D. Kidwell/NCEP	 9/99	Modified prologue                       *
C* D. Kidwell/NCEP	10/99	Allow either WWA or WW-A for status msg *
C* D. Kidwell/NCEP	 3/00	Changed icorr init; WW_RTIM -> DC_ITIM  *
C* D. Kidwell/NCEP	10/01	Changed prolog to add WWUS30, WOUS20    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, wnum, strtim, stptim, tissue, rwnum (*)
	INTEGER  	istarr (*)
	REAL   		rlat (*), rlon (*)
	LOGICAL		status
C*
	CHARACTER	watch*8000, bultim*6
	INTEGER  	iotarr (5)
C------------------------------------------------------------------------
	iret = -2
C
        tissue = ' '
	icorr  = 0
C
C*	Remove unprintables and control characters from bulletin and
C*	ensure upper case.
C
	CALL ST_UNPR ( bultin ( :lenbul ), lenbul, watch, lenw, ier )
	CALL ST_LCUC ( watch, watch, ier )
C
C*	Get issue time for watch or status report.
C
	IF ( .not. status ) THEN
	    ibpnt = INDEX ( watch ( :lenw ), ' AWW ' )
	    iend  = ibpnt + 10
	  ELSE
	    ibpnt = INDEX ( watch ( :lenw ), ' WW-A ' )
	    IF ( ibpnt .eq. 0 ) THEN
	        ibpnt = INDEX ( watch ( :lenw ), ' WWA ' )
	        iend  = ibpnt + 10
	      ELSE
	        iend  = ibpnt + 11
	    END IF
	END IF
C
	IF ( ibpnt .gt. 0 ) THEN
	    IF ( iend .le. lenw ) THEN
		bultim = watch ( iend-5:iend )
		CALL ST_INTG ( bultim, issue, iret )
	    END IF
	END IF
C
        IF ( iret .ge. 0 ) THEN
	    irday  = issue / 10000
            irhour = mod ( issue, 10000 ) / 100
            irmin  = mod ( mod ( issue, 10000 ), 100 )
            CALL DC_ITIM ( istarr, irday, irhour, irmin, iotarr, ier )
	    CALL TI_ITOC ( iotarr, tissue, ier )
	    IF ( ier .eq. 0 ) THEN
C
C*              Check for a correction.
C
                ibpnt = iend + 1
                IF ( ibpnt .lt. lenw ) THEN
                    indx = INDEX ( watch ( ibpnt:ibpnt+5 ), 'COR' )
                    IF ( indx .gt. 0 ) THEN
                        icorr = 1
                        ibpnt = ibpnt + indx + 2
                    END IF
                END IF
C
		IF ( .not. status ) THEN
C
C*	            Decode remainder of watch box report.
C
	            CALL WW_US40 ( watch, lenw, ibpnt, iotarr, mxp,
     +	  	                   itype, wnum, strtim, stptim, icancl,
     +	   	                   rlat, rlon, npt, irepl, rwnum, iret )
		  ELSE
C
C*	            Decode remainder of status report.
C
	            CALL WW_US8 ( watch, lenw, ibpnt, mxp, wnum, 
     +	   	             rlat, rlon, npt, irepl, rwnum ( 1 ), iret )
		    itype  = 3
		    icancl = npt
		END IF
C
C*		If this is a cancellation or a status report, substitute
C*		issue time for watch start and end times.
C
		IF ( status .or. ( icancl .eq. 1 ) ) THEN
		    strtim = tissue
		    stptim = tissue
		END IF
	      ELSE
		iret = -2
	    END IF
	END IF
C*
	RETURN
	END
