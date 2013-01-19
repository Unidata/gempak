	SUBROUTINE OANDTA  ( isnfln, nparms, rlevel, nlevel,
     +			     ilvert, nstnpm, ngrid, sndata, slat,
     +			     slon, cosslt, nstn, stnam, iret )
C************************************************************************
C* OANDTA								*
C*									*
C* This subroutine reads in the sounding data for OABSND.		*
C*									*
C* OANDTA  ( ISNFLN, NPARMS, RLEVEL, NLEVEL, ILVERT, NSTNPM, NGRID,	*
C*           SNDATA, SLAT, SLON, COSSLT, NSTN, STNAM, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN 		INTEGER		Sounding file number		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	RLEVEL (NLEVEL)	REAL		Levels				*
C*	NLEVEL		INTEGER		Number of levels		*
C*	ILVERT		INTEGER		Vertical coordinate		*
C*	NSTNPM		INTEGER		Number of station parameters	*
C*	NGRID		INTEGER		Number of grids			*
C*									*
C* Output parameters:							*
C*	SNDATA		REAL		Sounding data			*
C*	 (NGRID,NSTN)							*
C*	SLAT (NSTN)	REAL		Station latitude		*
C*	SLON (NSTN)	REAL		Station longitude		*
C*	COSSLT (NSTN)	REAL		Cosine of station latitude	*
C*	NSTN		INTEGER		Number of stations		*
C*	STNAM (*)	CHAR*		Station ID or number		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = too many stations		*
C*					 -7 = too few stations		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* K. Brill/GSC          4/90   Multiple sounding files			*
C* K. Brill/NMC          8/90   Fix multiple sounding files		*
C* K. Brill/NMC		06/91 	Added COSSLT as output			*
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* T. Lee/GSC		 3/99	Added STNAM in calling seq.		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( LLDTMX = LLSTFL * MMFILE )
	CHARACTER*(*)	stnam (*)
	REAL		sndata (*), slat (*), slon (*), cosslt (*)
	REAL		rlevel (*)
C*
	LOGICAL		done, good, found
	CHARACTER	stid*8, cstnm*8, cdata ( MMPARM )
	REAL		stndat ( LLMXDT )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	idup = 0
C*
	  done  = .false.
C
C* 	  Loop through each of the stations.
C
	  DO WHILE  ( .not. done )
C
C*	    Read in header for the next station.
C
	    CALL SN_SNXT  ( isnfln, stid, istnm,
     +                      rlat, rlon, relv, ier )
C
C*	    Check for end of file.
C
	    IF  ( ier .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*		Read in the data.
C
		CALL SN_RDAT  ( isnfln, numlev,
     +                          stndat, ihhmm, ier )
		IF  ( ier .eq. 0 )  THEN
C
C*		    Set station and increment station counter.
C
		    ispri = 0
		    CALL PC_SSTN  ( stid, istnm, rlat, rlon, relv,
     +				    ispri, ihhmm, numlev, ier )
		    IF  ( nstn .ge. LLDTMX )  THEN
			iret = -6
			CALL ER_WMSG  ( 'OABSND', iret, ' ', ier )
			RETURN
		      ELSE
C
C*			Check for duplicate station.
C
			iq = 1
			found = .false.
	                DO WHILE ( iq .le. nstn .and. .not. found )
	                  IF ( rlat .eq. slat (iq) .and.
     +                         rlon .eq. slon (iq) ) found = .true.
	                  iq = iq + 1
	                END DO
C
C*			Save station information.
C
			IF ( .not. found ) THEN
			  nstn = nstn + 1
			  slat (nstn) = rlat
			  cosslt (nstn) = COS ( rlat * DTR )
			  slon (nstn) = rlon
			  idata = ( nstn - 1 ) * ngrid + 1
			  good = .false.
C
C*			  Get level data.
C
			  IF  ( nparms .gt. 0 )  THEN
C
C*			    Loop through levels.
C
			    DO  il = 1, nlevel
				CALL PC_CMVR  ( rlevel (il), ilvert,
     +						stndat, 
     +						sndata ( idata ),
     +						cdata, ier )
				DO  ip = 0, nparms - 1
				    ii = idata + ip
				    IF  ( .not. ERMISS ( sndata (ii) ) )
     +						good = .true.
				END DO
				idata = idata + nparms
			    END DO
			  END IF
C
C*			  Get station parameters.
C
			  IF  ( nstnpm .gt. 0 )  THEN
			    CALL PC_CMST  ( stndat, sndata (idata),
     +					    cdata, ier )
			    DO  ip = 0, nstnpm - 1
				ii = idata + ip
				IF  ( .not. ERMISS ( sndata (ii) ) )
     +						good = .true.
			    END DO
			  END IF
		        ELSE
	 		  idup = idup + 1
	                END IF
C
C*			Save station ID or number.
C
			IF  ( good )  THEN
			  IF  ( stid .eq. ' ' )  THEN
			    CALL ST_INCH ( istnm, cstnm, ier )
			    IF  ( cstnm .ne. '999999' )  THEN
				stnam (nstn) = cstnm
			    ELSE
				stnam (nstn) = ' '
			    END IF
			  ELSE
			    stnam (nstn) = stid 
			  END IF
			ELSE
			  nstn = nstn - 1
			END IF
C
		    END IF
		END IF
	    END IF
	  END DO
C
C*	Check that there are at least four stations.
C
	IF  ( nstn .lt. 4 )  THEN
	    iret = -7
	    CALL ER_WMSG  ( 'OABSND', iret, ' ', ier )
	END IF
C*
	IF ( idup .ne. 0 ) THEN
	  WRITE (6, 1000) idup
1000	  FORMAT( ' ', I5, ' duplicate stations eliminated. ' )
	END IF
C*
	RETURN
	END
