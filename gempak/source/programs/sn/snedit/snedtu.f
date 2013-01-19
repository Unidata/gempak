	SUBROUTINE SNEDTU ( lunedt, ptdata, ptnam, iptime, zwind, nlev, 
     +                      nparts, iret )
C************************************************************************
C* SNEDTU								*
C*									*
C* This routine reads all the parts from an unmerged station.		*
C*									*
C* SNEDTU ( LUNEDT, PTDATA, PTNAM, IPTIME, ZWIND, NLEV, NPARTS, IRET )	*
C*									*
C* Input Parameters:							*
C*	LUNEDT		INTEGER		LUN for edit file		*
C*									*
C* Output Parameters:							*
C*	PTDATA(6*LLMXLV,12) REAL	Sounding data for station	*
C*      PTNAM(MXPART)	CHAR*           Station part names		*
C*	IPTIME(MXPART)	INTEGER		Part times in HHMM format	*
C*	ZWIND(MXPART)	LOGICAL		Flag for sig wind in Z coord	*
C*	NLEV(MXPART)	INTEGER		Number of levels for each part	*
C*	NPARTS		INTEGER		Number of station parts		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-19 = invalid station data	*
C**									*
C* Log:									*
C* S. Schotz/GSC	12/89						*
C* D. Kidwell/NCEP	 2/01	Increased number of parts from 6 to 12	*
C* D. Kidwell/NCEP	 4/05	Replaced 40 with MMPARM                 *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	PARAMETER 	(MXPART = 12, MXPARM = 6)
C*
	REAL 		ptdata(LLMXLV * MXPARM,MXPART)
	CHARACTER*(*)	ptnam(MXPART)
	INTEGER		iptime(MXPART), nlev(MXPART)
	LOGICAL		zwind(MXPART)
C*
	LOGICAL		done, zwflg, kntflg, ddpflg
        CHARACTER	partnm*6, record*132
	REAL		rarr (MMPARM)		
C------------------------------------------------------------------------
	iret = 0
	done = .false.
        nparts = 0
C
C*	Loop over all station parts
C
	DO WHILE ( .not. done )
C
C*	    Get part name and characteristics, check part validity
C
	    CALL SNECPT ( lunedt, partnm, itime, nparm, kntflg, ddpflg,
     +                    zwflg, ierr )
	    IF ( ierr .eq. 0) THEN
C
C*              Valid part
C
                nparts = nparts + 1
	        ptnam(nparts) = partnm
                iptime(nparts) = itime
	        zwind(nparts) = zwflg
                nlev(nparts) = 0
C
C*              Read in data for part
C
                iostat = 0
	        DO WHILE ( iostat .eq. 0 )
		   READ ( lunedt, 10, IOSTAT = iostat ) record
10		   FORMAT ( A )
	           IF ( iostat .eq. 0) THEN
		      CALL ST_LCUC ( record, record, jerr )
		      CALL ST_LDSP ( record, record, nchar, jerr )
		      IF ( nchar .gt. 0 ) THEN
C
C*                       Attempt to decode record into reals
C
                         CALL ST_C2R ( record, MXPARM, rarr, narr, 
     +                                 jerr )
                         IF ( jerr .eq. 0) THEN
			    IF ( narr .eq. nparm ) THEN
                               nlev(nparts) = nlev(nparts) + 1
C
C*                             Move data into output sounding data array
C
                               CALL SNEP2S ( rarr, partnm, nparm, 
     +                                       nlev(nparts), kntflg, 
     +                                       ddpflg, ptdata(1,nparts),
     +					     ier )
                            ELSE
C
C*                             Number of parameters invalid, write
C*                             error, skip to next part
C
                               CALL ER_WMSG ( 'SNEDIT', -17, partnm,
     +                                         jerr )
			       nparts = nparts - 1
                               iostat = -1
	                       iret = -19
                            END IF
                                                      
                         ELSE
C
C*                          Alpha numeric data, new part or station
C
                            iostat = -1
	                    CALL FL_BKSP ( lunedt, jerr )
                         END IF
                      END IF
                   END IF
	        END DO
C
            ELSE IF ( ierr .eq. 2 ) THEN
C
C*             New station
C
               CALL FL_BKSP ( lunedt, jerr )
	       done = .true.
C
	    ELSE IF ( ierr .eq. 3 ) THEN
C
C*             End of file
C
	       done = .true.
	    ELSE 
C
C*             Invalid part header, write error message continue
C	      
               CALL ER_WMSG ( 'SNEDIT', -17, partnm, jerr )
               iret = -19
            END IF
	END DO
C*
	RETURN
	END
