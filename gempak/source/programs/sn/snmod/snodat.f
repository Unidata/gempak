	SUBROUTINE SNODAT  ( isnfln, iflout, times, ntime, rlevel,
     +			     nlev, levtyp, lvert, nparms, iptype, 
     +                       idntyp, iret )
C************************************************************************
C* SNODAT								*
C*									*
C* This subroutine moves data from the input to the output file for	*
C* SNMOD.								*
C*									*
C* SNODAT  ( ISNFLN, IFLOUT, TIMES, NTIME, RLEVEL, NLEV, LEVTYP, 	*
C*           LVERT, NPARMS, IPTYPE, IDNTYP, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Input file number		*
C*	IFLOUT		INTEGER		Output file number		*
C*	TIMES (NTIME)	CHAR*		Times				*
C*	NTIME		INTEGER		Number of times			*
C*	RLEVEL (NLEV)	REAL		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	LEVTYP		INTEGER		Level type			*
C*	LVERT		INTEGER		Vertical coordinate number	*
C*	NPARMS		INTEGER		Number of output parameters	*
C* 	IPTYPE          INTEGER         Part type indicator		*
C* 					 0 = merged file		*
C*                                       1 = unmerged, man < 100 mb	*
C*                                       2 = unmerged, man, sig < mb	*
C*                                       3 = unmerged, all man and sig  *
C* 	IDNTYP		CHAR*		Station ID type to use		*
C*                                       STID = use ID string		*
C*					 STNM = use ID number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes to station time			*
C* S. Schotz/GSC	 1/90   Added write to unmerged file path	*
C* S. Schotz/GSC	 8/90	Added IDNTYP				*
C* K. Brill/NMC		 8/93	stid*4 -> stid*8			*
C* T. Lee/GSC		 9/97	Fixed typo, NLVEV3 -> NLEV3		*
C* A. Hardy/GSC 	 3/99	Added priority parameter to PC_SSTN     *
C* D. Kidwell/NCEP 	 2/01	Added more parts                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	times  (*), idntyp
	REAL		rlevel (*)
C*
	LOGICAL		good, zwind, datflg
	INTEGER		ihhmm (2)
	REAL		data (LLMXDT), rdata (LLMXDT)
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
		CALL ER_WMSG  ( 'SNMOD', -10, times (itime), ier )
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
C*                      Set station id appropriately.
C
			IF  ( idntyp .eq. 'STID' ) THEN
                            stn = stid
                        ELSE
			    CALL ST_INCH  ( id, stn, ier )
                        END IF
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
				CALL ER_WMSG  ( 'SNMOD', -11, stn, ier )
				good = .false.
			    END IF
			END IF
		    END IF
C
C*		    Read data and write data 
C
		    IF  ( good )  THEN
                        IF ( iptype .eq. 0) THEN
C
C*			    Merged data, read data send to PC package 
C*                          for parameter conversion and write.
C
			    CALL SN_RDAT  ( isnfln, numlev, data, ihhmm,
     +			                    iret )
			    ispri = 0
			    CALL PC_SSTN  ( stid, id, slat, slon, selv, 
     +					    ispri, ihhmm, numlev, iret )
			    CALL SNOCMP   ( nparms, rlevel, nlev, 
     +			                    levtyp, lvert, data, 
     +					    numlev, levout,rdata, iret )
			    IF  ( levout .gt. 0 )  THEN
			        CALL SN_WDAT  ( iflout, ihhmm (2), 
     +					        levout, rdata, ier )
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
                               CALL SN_RPRT ( isnfln, 'PPAA', ihhmm, 
     +                                      nlev2, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'PPAA', ihhmm, 
     +                                     nlev2, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'TRPA', ihhmm, 
     +                                      nlev3, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TRPA', ihhmm, 
     +                                     nlev3, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'MXWA', ihhmm, 
     +                                      nlev4, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'MXWA', ihhmm, 
     +                                     nlev4, ptdata, zwind, iret ) 
                            END IF
                            IF ( iptype .ge. 2) THEN
                               CALL SN_RPRT ( isnfln, 'TTBB', ihhmm, 
     +                                      nlev5, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTBB', ihhmm, 
     +                                     nlev5, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'PPBB', ihhmm, 
     +                                      nlev6, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'PPBB', ihhmm, 
     +                                     nlev6, ptdata, zwind, iret ) 
                            END IF
                            IF ( iptype .eq. 3) THEN
                               CALL SN_RPRT ( isnfln, 'TTCC', ihhmm, 
     +                                      nlev7, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTCC', ihhmm, 
     +                                     nlev7, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'TTDD', ihhmm, 
     +                                      nlev8, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TTDD', ihhmm, 
     +                                     nlev8, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'PPDD', ihhmm, 
     +                                      nlev9, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'PPDD', ihhmm, 
     +                                     nlev9, ptdata, zwind, iret ) 
                               CALL SN_RPRT ( isnfln, 'PPCC', ihhmm, 
     +                                     nlev10, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'PPCC', ihhmm, 
     +                                     nlev10, ptdata, zwind, iret )
                               CALL SN_RPRT ( isnfln, 'TRPC', ihhmm, 
     +                                     nlev11, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'TRPC', ihhmm, 
     +                                     nlev11, ptdata, zwind, iret )
                               CALL SN_RPRT ( isnfln, 'MXWC', ihhmm, 
     +                                     nlev12, ptdata, zwind, iret )
                               IF ( iret .eq. 0) 
     +                         CALL SN_WPRT ( iflout, 'MXWC', ihhmm, 
     +                                     nlev12, ptdata, zwind, iret )
                            END IF
                            nlv = 0
                            nlv = nlev1 + nlev2  + nlev3  + nlev4 +
     +                            nlev5 + nlev6  + nlev7  + nlev8 + 
     +				  nlev9 + nlev10 + nlev11 + nlev12
          	            IF ( nlv .gt. 0 ) nstn = nstn + 1
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
