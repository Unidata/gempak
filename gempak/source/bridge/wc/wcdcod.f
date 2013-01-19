	SUBROUTINE WC_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* WC_DCOD								*
C*									*
C* This subroutine decodes watch county notification bulletins and	*
C* writes tthe data to an ASCII file.					*
C*									*
C* WC_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*		IRET )    						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter table file name	*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 -4 = Unable to get bultim	*
C*					 -7 = Unable to get curtim	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03		Add bultim to WC_DECD call. seq.*
C* B. Yin/SAIC           3/04   	Changed SS_GTIM to CSS_GTIM     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, prmfil, stntbl
C*
	CHARACTER	bultin*(DCMXBF), parms(MMPARM)*4, errstr*80,
     +			filnam*132, dattim*11, bultim*12, 
     +			oristn*8, seqnum*4, wmohdr*8, 
     +			sysdt*12, dattmp*12, bbb*4
	CHARACTER       adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +                  stat(LLSTFL)*2, coun(LLSTFL)*2,
     +                  tbchrs(LLSTFL)*20
        INTEGER         istarr (5), irdtar (5), itype
        INTEGER         istnm(LLSTFL), ispri(LLSTFL)
        REAL            adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
	LOGICAL		more
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
 	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	iflsrc = 2 
C
C*	Read the county table.
C
	CALL DC_STNS  ( stntbl, adstn, istnm, stnnam, stat, coun, adlat,
     +          adlon, selv, ispri, tbchrs, nade, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL DC_WLOG ( 2, 'DC', ier, ' ', ierr )
	    RETURN
	END IF
C
C*	Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE ( iperr .eq. 0 )
	    icor = 0
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
		more = .true.
		CALL DC_TEST ( bultin, lenbul, itest, ier )
C
C*		Parse the header info from the bulletin.
C
                CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +                         oristn, bultim, bbb, nchar, ierr )
                IF (  bbb .eq. 'COR'  ) icor = 1
C
                IF ( ierr .lt. 0 ) THEN
		    CALL ST_UNPR ( bultin (:80), 80, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DC', ierr, errstr(:len1),
     +				   ier )
		    more = .false.
		END IF
C
                IF ( more ) THEN
C
C*                  Get the system time, and make a standard GEMPAK time
C*                  from the "current" time.
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
C*		    Set the GEMPAK date/time. Use bultim to set
C*		    the output file name.
C
		    CALL ST_LSTR ( bultim, len, ier )
		    IF ( len .ne. 6 ) THEN
			IF ( curtim .eq. 'SYSTEM' ) THEN
			    bultim = dattmp ( 5:6 ) // dattmp ( 8:11 )
			  ELSE
			    iret = -7
			    CALL ST_UNPR ( bultin (:80), 80, errstr,
     +				           len1, ier )
			    CALL DC_WLOG ( 2, 'DC', iret,
     +					   errstr(:len1), ier )
			    more = .false.
			END IF
		    END IF
C
		    IF ( more ) THEN
                        CALL ST_NUMB ( bultim, issue, ier )
                        irday  = issue / 10000
                        irhour = mod ( issue, 10000 ) / 100
                        irmin  = mod ( mod ( issue, 10000 ), 100 )
		        CALL DC_ITIM ( istarr, irday, irhour, irmin,
     +				   irdtar, ier )
		        CALL TI_ITOC ( irdtar, dattim, ier )
                        IF ( ier .lt. 0 ) more = .false.
C
C*		        Remove control characters from entire bulletin.
C
		        IF ( more ) THEN 
C
C*		            Set bultim from dattim, in case original
C*			    value was invalid.
C
		            bultim = dattim ( 5:6 ) // dattim ( 8:11 )
		            CALL ST_UNPR ( bultin, lenbul, bultin, 
     +					lenb, ier )
C
C*		            Make a file name from the template and the
C*		 	    time.  Open the file as part of the open
C*			    file list.
C
 		            CALL FL_MNAM ( dattim, gemfil, filnam, ier )
 		            CALL DC_FCYL ( filnam, iflsrc, stntbl,
     +				    iadstn, maxtim, lunf, nparm,
     +				    parms, ierr )
C
C*	                    Check that the file was opened properly.
C
		            IF ( ierr .ne. 0 ) THEN
C
C*	    	                If not, write an error to the decoder 
C*			        log file.
C
	    	                CALL DC_WLOG ( 0, 'SF', ierr, filnam,
     +					ier )
                                more = .false.
		            END IF
		        END IF
                    END IF
C
C*                  Decode the WCN segment
C
                    IF ( more) THEN
 
                        CALL WC_DECD ( bultin(:lenb), dattim, oristn,
     +                        icor, itest, adstn, istnm, stnnam, stat,
     +			      coun, adlat, adlon, selv, ispri, tbchrs, 
     +			      nade, lunf, bultim, ier )
                    END IF
                END IF 
 	        CALL DC_FCLS ( ier )
	    END IF
	END DO
C*
	RETURN
	END
