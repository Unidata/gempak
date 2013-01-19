	SUBROUTINE NC_DCOD  ( curtim, gemfil, stntbl, prmfil, iadstn,
     +			      maxtim, nhours, iret )
C************************************************************************
C* NC_DCOD								*
C*									*
C* This subroutine decodes WSUS[1|01|0] non-convective sigmet reports   *
C* (issued by KKCI, KBOS, KCHI, KDFW, KMIA, KSFO, and KSLC), and writes *
C* the data to an ASCII file.	                                        *
C*									*
C* NC_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*	      IRET )    						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter packing table         *
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	8/00	                                        *
C* D. Kidwell/NCEP     11/00	Removed unneeded error check            *
C* B. Yin/SAIC          3/04    Changed SS_GTIM to CSS_GTIM             *
C* J. Lewis/AWC		4/05	Add check for originating station KKCI	*
C*				and new WMO headers WSUS0,WCUS0,WVUS0	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF),
     +			seqnum*4, wmohdr*8, oristn*8, bultim*12,
     +			bbb*8, errstr*80, work*(DCMXBF), origin*4,
     +			sysdt*12, dattmp*12, tissue*20, org*3
	INTEGER		istarr (5), iotarr (5), itype
	LOGICAL		good
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
 	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
C
C*	Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
		good = .true.
		CALL DC_TEST ( bultin, lenbul, itest, ier )
		IF ( itest .eq. 2 ) itest = 3
C
C*		Parse the header info from the bulletin.
C
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +		                   oristn, bultim, bbb, nchar, ierr )
		  ELSE
		    ierr = -12
		END IF
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCNCON', 1, errstr, ier )
		    good = .false.
		END IF
C
C*		Check for bulletins from KBOS, KCHI, KDFW, KMIA, KSFO
C*		and KSLC only.
C*              This can be removed after the new WMO headers are effective.
C
		IF ( good ) THEN
		    origin = oristn ( :4 )
		    IF ( ( origin .eq. 'KBOS' ) .or.
     +		         ( origin .eq. 'KCHI' ) .or.
     +		         ( origin .eq. 'KDFW' ) .or.
     +		         ( origin .eq. 'KMIA' ) .or.
     +		         ( origin .eq. 'KSFO' ) .or.
     +		         ( origin .eq. 'KSLC' ) ) THEN
		        IF ( ( wmohdr ( :5 ) .ne. 'WSUS1') .and.
     +		             ( wmohdr ( :6 ) .ne. 'WSUS01' ) )
     +		             good = .false.
C
C*              Add the check for the new originating station
C*              effective with new WMO headers.
C
                      ELSE IF ( origin .eq. 'KKCI' ) THEN
                        IF ( ( wmohdr ( :5 ) .ne. 'WSUS0' ) .and.
     +			     ( wmohdr ( :5 ) .ne. 'WCUS0' ) .and.
     +			     ( wmohdr ( :5 ) .ne. 'WVUS0' ) )
     +                       good = .false.
		      ELSE
		        good = .false.
		    END IF
	 	END IF
C
		IF ( good ) THEN
		    org = origin ( 2:4 )
 	            IF ( ( bbb ( :3 ) .ne. 'COR' ) .and.
     +			 ( bbb ( :2 ) .ne. 'CC' ) ) THEN
			icorr = 0
		      ELSE
			icorr = 1
		    END IF
C
C*		    Set the bulletin pointer to one character past
C*		    the end of the WMO header.
C
		    ibpnt = nchar + 1
		    lenorg = lenbul
		    lenbul = lenbul - nchar
C
C*		    Remove unprintable characters.
C
		    CALL ST_UNPR ( bultin ( ibpnt:lenorg ), lenbul, 
     +				   work ( :lenbul ), lenb, ier )
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
C
		    itype = 1
		    CALL CSS_GTIM ( itype, sysdt, ier )
		    IF ( curtim .eq. 'SYSTEM' ) THEN
			dattmp = sysdt
		      ELSE
			CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		    END IF
		    CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		    Use the system time and the bulletin time to make a
C*		    Gempak time.
C
		    CALL ST_LSTR ( bultim, lens, ier )
		    IF ( lens .eq. 6 ) THEN
		        CALL ST_INTG ( bultim ( :6), issue, ierr )
			IF ( ierr .ne. 0 ) good = .false.
		      ELSE
			good = .false.
		    END IF
C
		    IF ( good ) THEN
	                irday  = issue / 10000
                        irhour = mod ( issue, 10000 ) / 100
                        irmin  = mod ( mod ( issue, 10000 ), 100 )
                        CALL DC_ITIM ( istarr, irday, irhour, irmin, 
     +				       iotarr, ier )
	                CALL TI_ITOC ( iotarr, tissue, ier )
		      ELSE
			CALL DC_WLOG ( 2, 'DC', -16, ' ', ier )
		    END IF
	        END IF
C
	        IF ( good ) THEN
C
C*		    Decode the non-convective sigmet report.
C
		    CALL NC_DECD ( work ( :lenb ), lenb, iotarr,
     +				   icorr, itest, tissue, gemfil, stntbl,
     +				   iadstn, maxtim, org, ier )
C
		END IF
	    END IF
	END DO
C*
	RETURN
	END
