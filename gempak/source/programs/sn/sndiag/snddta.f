	SUBROUTINE SNDDTA  ( isnfln, iflout, times, ntime, rlevel,
     +			     nlev, levtyp, lvert, nparms, iptype, 
     +                       idntyp, wavlen, wavspd, storm, crdrot,
     +			     tropht, trpint, cldhgt, mxdpth, squall,
     +			     delz, filtyp, spline, nlun, lun, parms,
     +			     iret )
C************************************************************************
C* SNDDTA								*
C*									*
C* This routine will read the data from the input file and write it 	*
C* to the output file.							*
C*									*
C* SNDDTA  ( ISNFLN, IFLOUT, TIMES, NTIME, RLEVEL, NLEV, LEVTYP, 	*
C*	     LVERT, NPARMS, IPTYPE, IDNTYP, WAVLEN, WAVSPD, STORM,	*
C*	     CRDROT, TROPHT, TRPINT, CLDHGT, MXDPTH, SQUALL, DELZ, 	*
C*	     FILTYP, SPLINE, NLUN, LUN, PARMS, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Input sounding file number	*
C*	IFLOUT		INTEGER		Output sounding file number	*
C*	TIMES		CHAR*		Input times			*
C*	NTIME		INTEGER		Number of times			*
C*	RLEVEL (NLEV)	REAL		List of levels			*
C*	NLEV	  	INTEGER		Number of levels		*
C*	LEVTYP		INTEGER		Type of level specification	*
C*					  1 = list of levels		*
C*					  2 = range without increment	*
C*	LVERT		INTEGER		Vertical coordinate		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	IPTYPE 		INTEGER		Sounding part type		*
C*					  0 = unmerged			*
C*					>=1 = merged type		*
C*	IDNTYP		INTEGER		Station id type			*
C*	WAVLEN		CHAR*		Length of wave			*
C*	WAVSPD		CHAR*		Speed of wave			*
C*	STORM		CHAR*		Storm direction and speed	*
C*	CRDROT		CHAR*		Coordinate rotation		*
C*	TROPHT		CHAR*		Tropopause height		*
C*	TRPINT		CHAR*		Interval above and below trop	*
C*	CLDHGT		CHAR*		Height of cloud base		*
C*	MXDPTH		CHAR*		Depth of the mixing layer	*
C*	SQUALL		CHAR*		Length of squall line		*
C*	DELZ		CHAR*		Distance for height increment	*
C*	FILTYP		CHAR*		Filter type			*
C*	SPLINE		LOGICAL		Whether to use splines or not	*
C*	NLUN		INTERGER	Number of output file numbers	*
C*	LUN (NLUN)	INTEGER		Output file numbers		*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Copied from SNODAT		*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	times(*), idntyp, wavlen, wavspd, storm, crdrot,
     +			tropht, trpint, cldhgt, mxdpth, squall, delz,
     +			filtyp, parms(*)
	REAL		rlevel(*)
	INTEGER		lun(*)
	LOGICAL		spline
C*
	LOGICAL		good, zwind, datflg
	REAL		data (LLMXDT), hdata (LLMXDT)
        REAL		ptdata (LLMXLV*6)
	CHARACTER	stat*4, coun*4, stid*8, stn*8
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through times.
C
	DO  itime = 1, ntime
C
C*	    Set time in input file.
C
	    CALL SN_STIM  ( isnfln, times (itime), iret )
	    CALL SN_BEGS  ( isnfln, ier )
C
C*	    Attempt to set time in output file.  If time is not there,
C*	    add time to file.
C
	    CALL SN_STIM  ( iflout, times (itime), ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL SN_ATIM  ( iflout, times (itime), ier )
	        CALL SN_STIM  ( iflout, times (itime), ierr )
	    END IF
C
C*	    Check for error in adding time.
C
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SNDIAG', -10, times (itime), ier )
	    ELSE
C
C*		Loop through stations.
C
		nstn = 0
		iout = 0
		DO WHILE  ( iout .eq. 0 )
C
C*		    Select next station.
C
		    CALL SN_SNXT  ( isnfln, stid, id, slat, slon, selv, 
     +				    iout )
C
C*		    Check for station data.
C
		    good = .true.
		    IF  ( iout .ne. 0 )  THEN
			good = .false.
		    ELSE
                        CALL SN_QDAT ( isnfln, datflg, iret)
			IF  ( iret .ne. 0 .or. (.not. datflg) )
     +                        good = .false.
		    END IF
C
C*		    Set station in output file.  If not found, add it.
C
		    IF  ( good )  THEN
C
C*                      Set station id appropiately.
C
			IF  ( idntyp .eq. 'STID' ) THEN
                            stn = stid
                        ELSE
			    CALL ST_INCH  ( id, stn, ier )
                        END IF
			DO  jjj = 1, nlun
			    WRITE ( lun(jjj), 2000 ) stn, times(itime)
			END DO
2000			FORMAT ( /'OUTPUT FOR ', A, ' FOR ', A )
			CALL SN_SSTN  ( iflout, stn, stid, iid, rlat,
     +					rlon, relv, ier )
			IF  ( ier .ne. 0 )  THEN
			    CALL SN_QSTN  ( isnfln, stid, id, slat, 
     +					    slon, selv, stat, coun, ir )
			    CALL SN_ASTN  ( iflout, 1, stid, id, slat, 
     +					    slon, selv, stat, coun, n, 
     +					    ier )
			    IF  ( ier .eq. 0 )  THEN
				CALL SN_SSTN  ( iflout, stn, stid, id,
     +						slat, slon, selv, ier )
			      ELSE
				CALL ER_WMSG  ( 'SNDIAG', -11, stn, ier )
				good = .false.
			    END IF
			END IF
		    END IF
C
C*		    Read data and write data 
C
		    IF  ( good )  THEN
			IF  ( iptype .eq. 0) THEN
C
C*			    Merged data, read data send to PC package 
C*                          for parameter conversion and write.
C
			    CALL SN_RDAT ( isnfln, numlev, data, ihhmm,
     +			                   iret )
			    CALL PC_SSTN ( stid, id, slat, slon, selv, 
     +					   ispri, ihhmm, numlev, iret )
			    CALL SNDCMP ( nparms, rlevel, nlev, 
     +			                  levtyp, lvert, data, 
     +					  numlev, parms,
C*			    Input for special functions.
     +					  wavlen, wavspd, storm, crdrot,
     +					  tropht, trpint, cldhgt,
     +					  mxdpth, squall, delz, filtyp,
     +					  spline, nlun, lun,
     +					  times(itime),
C*			    Output from SNDCMP.
     +					  nlevel, hdata, iret )
			    IF  ( iret .ne. 0 )  RETURN
C
			    IF  ( nlevel .gt. 0 )  THEN
			        CALL SN_WDAT  ( iflout, ihhmm, 
     +					        nlevel, hdata, ier )
			        nstn = nstn + 1
			    END IF
                        ELSE
C
C*                          Unmerged data, read and write parts
C
                            IF ( iptype .ge. 1) THEN
                               CALL SN_RPRT ( isnfln, 'TTAA', ihhmm, 
     +                                      nlev1, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTAA', ihhmm, 
     +                                     nlev1, ptdata, zwind, iret ) 
                            END IF
                            IF ( iptype .ge. 2) THEN
                               CALL SN_RPRT ( isnfln, 'TTBB', ihhmm, 
     +                                      nlev2, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTBB', ihhmm, 
     +                                     nlev2, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'PPBB', ihhmm, 
     +                                      nlev3, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'PPBB', ihhmm, 
     +                                     nlev3, ptdata, zwind, iret ) 
                            END IF
                            IF ( iptype .eq. 3) THEN
                               CALL SN_RPRT ( isnfln, 'TTCC', ihhmm, 
     +                                      nlev4, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTCC', ihhmm, 
     +                                     nlev4, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'TTDD', ihhmm, 
     +                                      nlev5, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTDD', ihhmm, 
     +                                     nlev5, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'PPDD', ihhmm, 
     +                                      nlev6, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'PPDD', ihhmm, 
     +                                     nlev6, ptdata, zwind, iret ) 
                            END IF
                            nlev = 0
                            nlev = nlev1 + nlev2 + nlvev3 + nlev4 +
     +                             nlev5 + nlev6
          	            IF ( nlev .gt. 0) nstn = nstn + 1
                        END IF
		    END IF
		END DO
C*
		WRITE  ( 6, 1000, IOSTAT = iostat ) nstn, times (itime)
1000		FORMAT ( 1X, I5, ' stations were added for time ', A )
	    END IF
	END DO
C*
	RETURN
	END
