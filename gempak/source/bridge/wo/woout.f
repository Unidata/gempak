	SUBROUTINE WO_OUT ( lunf, hdln, wtype, strtim, stptim, stmid, 
     +               origin, wfos, icor, icancl, cnties, ncnty, adstn, 
     +		     istnm, stnnam, stat, coun, adlat, adlon, selv, 
     +		     ispri, tbchrs, nade, tissue, zone, iret )
C************************************************************************
C* WO_OUT								*
C*									*
C* This subroutine writes the watch ouline update message type, start   *
C* time, stop time, watch number, bulletin originator, wfo stations,    *
C* correction/test flag, cancellation flag, and county information to   *
C* an ASCII file.							*
C*									*
C* WO_OUT ( LUNF, WTYPE, STRTIM, STPTIM, STMID, ORIGIN, WFOS, ICOR,     *
C*	    ICANCL, CNTIES, NCNTY, ADSTN, ISTNM, STNNAM, STAT, COUN,    *
C*	    ADLAT, ADLON, SELV, ISPRI, TBCHRS, NADE, TISSUE, ZONE, IRET)*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	HDLN		LOGICAL		Flag for the data line		*
C*	WTYPE		CHAR*		Watch message type		*
C*	STRTIM		CHAR*		Start time			*
C*	STPTIM		CHAR*		Stop time			*
C*	STMID		CHAR*		Watch number			*
C*	ORIGIN		CHAR*		Bulletin originator		*
C*	WFOS		CHAR*		String of active WFOs		*
C*	ICOR		INTEGER		Correction indicator		*
C*	ICANCL          INTEGER         Cancellation flag		*
C*	CNTIES(NCNTY)	CHAR*6		County names in watch area    	*
C*	NCNTY		INTEGER		Number of counties in area	*
C*	ADSTN (*)       CHAR*           Station ID                      *
C*      ISTNM (*)       INTEGER         Station number                  *
C*      STNNAM (*)      CHAR*           Station name                    *
C*      STAT (*)        CHAR*           State ID                        *
C*      COUN (*)        CHAR*           Country ID                      *
C*      ADLAT (*)       REAL            Latitude                        *
C*      ADLON (*)       REAL            Longitude                       *
C*      SELV (*)        REAL            Elevation                       *
C*      ISPRI (*)       INTEGER         Priority number                 *
C*      TBCHRS (*)      CHAR*           Issuing station name            *
C*      NADE            INTEGER         Number of table entries         *
C*	TISSUE		CHAR*		Bulletin issue time		*
C*	ZONE		CHAR*		WOU time zone			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					-12 = length of header > 120	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03		Added 'tissue' to output string *
C* A. Hardy/NCEP	 2/03		Increased wloc 80->120		*
C* A. Hardy/NCEP	 2/03		Write out time zone		*
C* F. J. Yen/NCEP	10/06		Checked length of wloc		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype, strtim, stptim, origin, cnties(*),
     +                  wfos, stmid, tissue, zone
	CHARACTER*(*)   adstn(*), stnnam(*), stat(*), coun(*),
     +                  tbchrs(*)
        INTEGER         istnm(*), ispri(*), nade
        REAL            adlat(*), adlon(*), selv(*)
        LOGICAL         hdln
C*
	CHARACTER       wloc*120, corr, cancl
C-----------------------------------------------------------------------
	iret = 0
C
        IF ( .not. hdln ) THEN
C
C*	    Setting up the information string.
C
 	    CALL ST_LSTR ( wtype, lenwtyp, ier )
	    CALL ST_LSTR ( strtim, lentim, ier )
	    CALL ST_LSTR ( stmid, leni, ier )
	    CALL ST_LSTR ( wfos, lenw, ier )
	    CALL ST_INCH ( icor, corr, ier )
	    CALL ST_INCH ( icancl, cancl, ier )
	    CALL ST_LSTR ( corr, lencor, ier )
	    CALL ST_LSTR ( origin, lenstn, ier )
	    CALL ST_LSTR ( zone, lenz, ier )

 	    wloc = '|' // wtype(:lenwtyp) // '|' // tissue(:lentim) //
     +	      	   '|' // strtim(:lentim) // 
     +	      	   '|' // stptim(:lentim) // '|' // stmid(:leni) //
     +	      	   '|' // origin(:lenstn) // '|' // wfos(:lenw) // 
     +    	   '|' // zone(:lenz)     // '|' // corr(:lencor) // 
     +             '|' // cancl
C
C*	    Write the storm information to the ASCII file. 
C
	    CALL ST_LSTR ( wloc, lenstr, ier)
	    IF ( lenstr .le. 120 ) THEN
	        WRITE(lunf,20)wloc
 20             FORMAT (A)
	      ELSE
		iret = -12
		RETURN
	    END IF
        END IF
C
C*	Compare the county ids with the county table.
C
	DO istn = 1, nade 
	    DO icnt = 1, ncnty
	        IF ( cnties(icnt) .eq. adstn(istn) ) THEN
	            WRITE(lunf,100) adstn(istn), istnm(istn), 
     +			    stnnam(istn), stat(istn), coun(istn), 
     +			    adlat (istn), adlon (istn), selv(istn), 
     +                      ispri(istn), tbchrs(istn)
 100		    FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                    F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
		END IF
	    END DO
	END DO
C*
	RETURN
	END
