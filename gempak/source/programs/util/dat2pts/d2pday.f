	SUBROUTINE D2PDAY ( lun, date, path, iday, otlktyp, nmfg,
     +                      idaymnth, iret )
C************************************************************************
C* D2PDAY								*
C*									*
C* This subroutine processes the files for the DAY 1, 2, or 3  Outlook/	*
C* fire weather product or DAY 3-8 fire weather product or DAY 4-8 SVR	*
C* weather outlook product depending on the value for iday and otlktyp.	*
C*									*
C* D2PDAY ( LUN, DATE, PATH, IDAY, OTLKTYP, IRET )			*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Output file unit number		*
C*	DATE		CHAR*		User input date for file name	*
C*	PATH		CHAR*		Directory path to the data files*
C*	IDAY		INTEGER		Day of the Outlook prod. (1,2,3,*
C*					38,48)				*
C*      OTLKTYP         CHAR*           Type of outlook, C=Convective   *
C*                                      F=Fire Weather                  *
C*	NMFG		CHAR*		Specify product issuance during	*
C*					the day for ENH-TSTM OTLKS	*	
C*	IDAYMNTH	CHAR*		Day of month	
C*					for Enhanced TSTM Outlooks	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	11/04	Generalized D2PDAY1, D2PDAY2, & D2PDAY3 *
C* G. Grosshans         01/06   Updated to include fire weather outlooks*
C* F. J. Yen/NCEP	10/07	Added Day 3-8 fire wx & Day 4-8 svr wx  *
C* G. Grosshans/SPC	02/10	Added Enh TSTM outlooks			*
C* G. Grosshans/SPC     12/13   Updated Day 4-8 processing              *
C* S. Guan/NCEP         06/19   Made DAY2 input with Hail, Wind and     *
C*                              Tornado probabilities (similar to DAY1) *
C*                              works.                                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NFILE = 5 )
C*
	CHARACTER*(*)	date, path, nmfg, idaymnth
C*
	CHARACTER	filtyp(NFILE,6)*30, dattyp(NFILE,6)*30,
     +			edate(NFILE,6)*30,
     +			filnam*(MXFLSZ), buffer*(LLMXLN),
     +			range*(LLMXLN), infile*(MXFLSZ),
     +			fcstr*(LLMXLN), chday(9)*2, chday2(9)*3
C*     +			fcstr*(LLMXLN), chday(5)*2, chday2(5)*3
        CHARACTER       otlktyp*1 
        CHARACTER       idate*2, ndate*2, ehour*4
	LOGICAL		found, catflg
C*
	DATA		filtyp / 'tornoutlook', 'hailoutlook',
     +				 'windoutlook', 'proboutlook', 
     +                           'outlook',
     +				 'proboutlook', 'outlook', 3*' ', 
     +				  'fireoutlook',  4*' ',
     +				 'fireoutlook',  4*' ',
     +				 'ext_svroutlook', 4*' ',
     +				 'enh16outlook', 'enh20outlook',
     +				 'enh00outlook', 'enh04outlook',
     +				 'enh12outlook'/
	DATA		dattyp / 'TORNADO', 'HAIL', 'WIND',
     +				 'PROB', 'CATEGORICAL', 
     +                           'ANY SEVERE', 'CATEGORICAL', 3*' ', 
     +				 'FIRE WEATHER CATEGORICAL', 4*' ',
     +				 'CRITICAL FIRE WEATHER AREA', 4*' ',
     +				 'ANY SEVERE', 4*' ',
     +                           5*'THUNDERSTORM OUTLOOK' /
        DATA		edate / 5*' ',
     +				5*' ',
     +				5*' ',
     +				5*' ',
     +				5*' ',
     +				'1200', '1600', '2000', '0000',
     +				'0400' /
C*	DATA		chday  / '1', '2', '3', '38', '48' /
C*	DATA		chday2 / '1', '2', '3', '3-8', '4-8' /
	DATA		chday  / '1', '2', '3', '38', '4', '5', '6', '7', '8' /
	DATA		chday2 / '1', '2', '3', '3-8', '4', '5', '6', '7', '8' /
C------------------------------------------------------------------------
	iret = 0
	fcstr = ' '
C
C*	Get the necessary string lengths.
C
	CALL ST_LSTR ( date, lend, ier )
	CALL ST_LSTR ( path, lenp, ier )
	CALL ST_LSTR ( idaymnth, lenm, ier )
C*
        idate = date(1:2)
        ndate = idaymnth(1:2) 
        ehour = date(3:6)
C
C*	Loop over all of the input files.
C*	nfil is number of input files
C*      ndx is subgroup into the DATA ARRAY, where
C*	each type of outlook is a subgroup.
C*      inumndx is only used for enhance tstm otlks.
C*	and is a way to access the correct 
C*      filenames at the appropriate issuance times.
C*
C*      NMFG is used to specify what product issuance during
C*	the day is being dealt with.  
C*  	
C*  	      ISSUANCE   BEGINNING ISSUANCE
C*      NMFG: TIME:      TIMES:
C*         1   06Z       1200  1600  2000
C*         2   13Z             1600  2000  0000
C*         3   17Z                   2000  0000  0400
C*         4   20Z                         0000  0400
C*         5   01Z                               0400
C
C
C*	    otlktyp is 'Convective'
C
        IF ( otlktyp .eq. 'C' ) THEN
	    IF  (iday .le. 2 ) THEN
	        nfil = 5
	        ndx  = 1
                inumndx = 0
	      ELSE IF ( iday .eq. 48 ) THEN
	        nfil = 5
	        ndx  = 5
                inumndx = 0
	      ELSE
	        nfil = 2
	        ndx  = 2
                inumndx = 0
	    END IF
C
C*	    otlktyp is 'Fire'
C
        ELSE IF ( otlktyp .eq. 'F' ) THEN
	    IF ( iday .eq. 38 ) THEN
	        nfil = 1
	        ndx  = 4
                inumndx = 0
	      ELSE
		nfil = 1
	        ndx  = 3
                inumndx = 0
	    END IF
C
C*	    otlktyp is 'Enhanced Thunderstorm Outlook'
C
        ELSE IF ( otlktyp .eq. 'E' ) THEN
	    IF ( nmfg .eq. '1' ) THEN
                inumndx = 0
	        nfil = 3
	        ndx  = 6
	      ELSE IF ( nmfg .eq. '2' ) THEN
                inumndx = 1
		nfil = 3
	        ndx  = 6
	      ELSE IF ( nmfg .eq. '3' ) THEN
                inumndx = 2
		nfil = 3
	        ndx  = 6
	      ELSE IF ( nmfg .eq. '4' ) THEN
                inumndx = 3
		nfil = 2
	        ndx  = 6
	      ELSE IF ( nmfg .eq. '5' ) THEN
                inumndx = 4
		nfil = 1
	        ndx  = 6
	    END IF
	END IF

	DO  i = 1, nfil
C
C*	    Get the necessary string lengths.
C
            IF ( otlktyp .ne. 'E' .and. iday .eq. 48 .and. 
     +           i .ge. 2 ) THEN   
	      CALL ST_LSTR ( dattyp(1,ndx), lent, ier )
	      CALL ST_LSTR ( filtyp(1,ndx), lenf, ier )
            ELSE
	      CALL ST_LSTR ( dattyp(i,ndx), lent, ier )
	      CALL ST_LSTR ( filtyp(i,ndx), lenf, ier )
            END IF
C
C*	    Create the input file name.
C*          For Enhanced TSTM outlooks its a day 1 so
C*          no special changes needed in terms of
C*          jday, lday and l2day.
C
	    IF ( iday .eq. 38 ) THEN
		jday = 4
	        lday = 2
	        l2day = 3
	      ELSE IF ( iday .eq. 48 ) THEN
C*		jday = 5	
C*	        lday = 2
                jday = i + 4
	        lday = 1
	        l2day = 3
	      ELSE
		jday = iday
	        lday = 1
	        l2day = 1
	    END IF

C*
C*          Handle Day 4-8 first
C*
            IF ( otlktyp .ne. 'E' .and. iday .eq. 48 .and. 
     +           i .ge. 2 ) THEN   
               filnam = 'ext_svroutlook_DAY' //
     +         chday(jday)(:lday) // '_' // date(:lend) // 'Z.dat'
            ELSE IF ( otlktyp .ne. 'E' ) THEN
	       filnam = filtyp(i+inumndx,ndx)(:lenf) // '_DAY' // 
     +	       chday(jday)(:lday) // '_' // date(:lend) // 'Z.dat'

            ELSE
C*
C*	       ENHANCED TSTM OUTLOOK
C*
	       IF ( i .eq. 1 ) THEN 
		   filnam = filtyp(i+inumndx,ndx)(:lenf) // '_DAY' // 
     +	           chday(jday)(:lday) // '_' // date(:lend) // 'Z.dat'
               ELSE IF ( ( i .eq. 2 .and. nmfg .eq. '1' ) .OR.
     +                   ( i .eq. 3 .and. nmfg .eq. '1' ) .OR.
     +                   ( i .eq. 2 .and. nmfg .eq. '2' ) .OR.
     +                   ( i .eq. 2 .and. nmfg .eq. '4' )) THEN 
		   filnam = filtyp(i+inumndx,ndx)(:lenf) // '_DAY' // 
     +	           chday(jday)(:lday) // '_' // 
     +             idate(:2) // edate(i+inumndx,ndx)(:4) // 'Z.dat'
               ELSE IF ( ( i .eq. 3 .and. nmfg .eq. '2' ) .OR.
     +                   ( i .eq. 2 .and. nmfg .eq. '3' ) .OR.
     +                   ( i .eq. 3 .and. nmfg .eq. '3' ) .OR.
     +                   ( i .eq. 2 .and. nmfg .eq. '2' )) THEN 
		   filnam = filtyp(i+inumndx,ndx)(:lenf) // '_DAY' // 
     +	           chday(jday)(:lday) // '_' // 
     +             ndate(:2) // edate(i+inumndx,ndx)(:4) // 'Z.dat'
	       END IF
            END IF
	    infile = path(:lenp) // '/' // filnam
C
C*	    Open the input file.
C
	    CALL FL_SOPN ( infile, lunin, ierr )
	    IF  ( ierr .eq. 0 )  THEN
C
C*		Skip the first line.
C*		The second line is the forecaster's name. Only use the
C*		name from the first file.
C
		READ ( lunin, 2000 ) buffer
		IF  ( fcstr .eq. ' ' )  THEN
		    READ ( lunin, 2000 ) fcstr
		  ELSE
		    READ ( lunin, 2000 ) buffer
		END IF
2000	    	FORMAT ( A )
C
C*	    	Find the entry that has the time range for the data.
C
		found = .false.
		DO WHILE  ( .not. found )
		    READ ( lunin, 2000 ) buffer
		    ipos = INDEX ( buffer, 'Z -' )
		    IF  ( ipos .ne. 0 )  THEN
			found = .true.
			range = buffer
			CALL ST_LSTR ( range, lenr, ier )
		    END IF
		END DO
C
C*	    	If this is the first file, write the valid times to the
C*	    	output file.  If this is ENH-TSTM product then write
C*		valid times for each time-period/file.
C*C
		IF  ( (i .ge. 2 ) .AND. ( otlktyp .eq. 'E' ))  THEN
		    WRITE ( lun, 1009 )
1009		    FORMAT ( ' ', A , / )
                END IF
C
		IF  ( (i .eq. 1 ) .OR. ( otlktyp .eq. 'E' ))  THEN
		    WRITE ( lun, 1010 ) range(:lenr)
1010		    FORMAT ( 'VALID TIME ', A , / )
C
                    IF ( otlktyp .eq. 'C' ) THEN
		       IF ( iday .eq. 48 ) THEN
       		           WRITE ( lun, 1020 ) chday2 (jday) (:l2day)
1020		           FORMAT ( 'SEVERE WEATHER OUTLOOK POINTS DAY ', a )
			 ELSE
       		           WRITE ( lun, 1021 ) chday2 (jday) (:l2day)
1021		           FORMAT ( 'PROBABILISTIC OUTLOOK POINTS DAY ', a1 )
		       END IF
                    END IF 
                    IF ( otlktyp .eq. 'F' ) THEN
       		       WRITE ( lun, 1025 ) chday2 (jday) (:l2day)
1025		       FORMAT ( 'FIRE WEATHER OUTLOOK POINTS DAY ', a )
                    END IF 
                    IF ( otlktyp .eq. 'E' ) THEN
       		       WRITE ( lun, 1026 ) chday2 (jday) (:l2day)
1026		       FORMAT ( 'THUNDERSTORM OUTLOOK POINTS DAY ', a )
                    END IF 
		    catflg = .false.
C
C*	          If this is the last file  write the
C*		  Categorical header to the output file and set the
C*		  categorical flag catflg.  (Since it is not the first
C*		  file, it won't be for an extended outlook.)
C
                ELSE IF ( otlktyp .ne. 'E' .and. iday .eq. 48 .and. 
     +                    i .ge. 2 .and. i .le. nfil ) THEN   
C*                 DAY 4-8 SEVERE WEATHER OUTLOOK POINTS LABEL     
		   WRITE ( lun, 1009 )
       		   WRITE ( lun, 1020 ) chday2 (jday) (:l2day)

                ELSE IF  ( (i .eq. nfil) .and. iday .ne. 48 .and.
     +                     (otlktyp .eq. 'C') ) THEN
		   WRITE ( lun, 1040 ) chday2 (iday)
1040		   FORMAT ( /, 'CATEGORICAL OUTLOOK POINTS DAY ', a1 )
		   catflg = .true.
		END IF
C
C*	    	Write the data type to the output file.
C
                IF ( otlktyp .ne. 'E' .and. iday .eq. 48 .and. 
     +               i .ge. 2 ) THEN   
		  WRITE ( lun, 1030 ) dattyp(1,ndx)(:lent)
                ELSE
		  WRITE ( lun, 1030 ) dattyp(i,ndx)(:lent)
                END IF
1030	    	FORMAT ( /, '... ', A, ' ...', / )
C
C*	    	Process the labels and points of the outlook lines.
C
		CALL D2PFIL ( lunin, lun, catflg, otlktyp, ier )
C
C*		If this is the last file, write the forecaster's name.
C*		This is left in for future use.
C
		IF  ( i .eq. nfil )  THEN
		    CALL ST_LSTR ( fcstr, lenc, ier )
C--		    WRITE ( lun, 1050 ) fcstr(:lenc)
C1050		    FORMAT ( /, 'FORECASTER...', A )
		END IF
C
C*	    	Close the input file.
C
		CALL FL_CLOS ( lunin, ier )
	    END IF
     	END DO
C*
	RETURN
	END
