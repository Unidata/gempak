	SUBROUTINE WS_OUT  ( lunf, wtype, strtim, stptim, origin, wthr,
     +	 icor, icancl, nhdln, zones, nzone, adstn, istnm, stnnam,
     +		 stat, coun, adlat, adlon, selv, ispri,
     +		 tbchrs, nade, iret )
C************************************************************************
C* WS_OUT								*
C*									*
C* This subroutine writes the winter storm message type, start time, 	*
C* stop time, bulletin originator, weather type, correction/test flag	*
C* and cancellation flag, and zone information to an ASCII file.	*
C*									*
C* WS_OUT ( LUNF, WTYPE, STRTIM, STPTIM, ORIGIN, WTHR, ICOR, ICANCL,	*
C*	   NHDLN, ZONES, NZONE, ADSTN, ISTNM, STNNAM, STAT, COUN, ADLAT,*
C*	   ADLON, SELV, ISPRI, TBCHRS, NADE, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	WTYPE  (2)	CHAR*		Winter storm message type	*
C*	STRTIM (2)	CHAR*		Start time			*
C*	STPTIM (2)	CHAR*		Stop time			*
C*	ORIGIN		CHAR*		Bulletin originator		*
C*	WTHR   (2)	CHAR*		Weather type			*	
C*	ICOR		INTEGER		Correction indicator		*
C*	ICANCL (2)      INTEGER         Cancellation flag		*
C* 	NHDLN		INTEGER		Number of headlines		*
C*	ZONES (NZONE)	CHAR*6		Zones names in storm area    	*
C*	NZONE		INTEGER		Number of zones in area		*
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
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02						*
C* M. Li/SAIC		02/03	Increase dim to handle 2 headlines	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wtype(*), strtim(*), stptim(*), origin, wthr(*)
	CHARACTER*(*)   zones (*), adstn (*), stnnam (*), stat (*), 
     +   		coun (*), tbchrs (*)
        INTEGER         istnm (*), ispri (*), nade, icancl(*)
        REAL            adlat (*), adlon (*), selv (*)
C*
	CHARACTER       wloc*80, corr, cancl
C-----------------------------------------------------------------------
	iret = 0
C
C*	Setting up the information string.
C
	CALL ST_LSTR ( wtype(1), lenwtyp, ier )
	CALL ST_LSTR ( strtim(1), lentim, ier )
	CALL ST_INCH ( icor, corr, ier )
	CALL ST_LSTR ( origin, lenstn, ier )

       	DO ii = 1, nhdln
	   CALL ST_INCH ( icancl(ii), cancl, ier )
	   CALL ST_LSTR ( wthr(ii), lenw, ier )
	   wloc = '|' // wtype(ii)(:lenwtyp) // '|' // 
     +            strtim(ii)(:lentim) // '|' // stptim(ii)(:lentim) // 
     +            '|' // origin(:lenstn) //
     +    	  '|' // wthr(ii)(:lenw) // '|' // corr // 
     +	          '|' // cancl
C
C*	   Write the storm information to the ASCII file. 
C
	   WRITE(lunf,20)wloc
 20        FORMAT (A)
C
C*	   Compare the zones ids with the zones table.
C
	   DO istn = 1, nade 
	      DO icnt = 1, nzone
	        IF ( zones(icnt) .eq. adstn(istn) ) THEN
	            WRITE(lunf,100) adstn(istn), istnm(istn), 
     +			    stnnam(istn), stat(istn), coun(istn), 
     +			    adlat (istn), adlon (istn), selv(istn), 
     +                      ispri(istn), tbchrs(istn)
 100		    FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                    F9.2, 1X, F9.2, 1X, F9.2, 1X, I2, 1X, A )
		END IF
	      END DO
	   END DO
	END DO
C*
	RETURN
	END
