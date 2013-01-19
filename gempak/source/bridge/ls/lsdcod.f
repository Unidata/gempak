        SUBROUTINE LS_DCOD ( curtim, gemfil, stntbl, prmfil,
     +			     iadstn, maxtim, nhours, iret )
C************************************************************************
C* LS_DCOD                                                              *
C*                                                                      *
C* This subroutine will read land surface synoptic observational data   *
C* bulletins from standard input, decode the bulletin report data,      *
C* and write the output to GEMPAK files.                                *
C*                                                                      *
C* LS_DCOD ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,    *
C*           IRET )                                                     *
C*                                                                      *
C* Input parameters:                                                    *
C*      CURTIM         CHAR*             Date/time from command line    *
C*      GEMFIL         CHAR*             Output file name template      *
C*      STNTBL         CHAR*             Synoptic land station table    *
C*      PRMFIL         CHAR*             Parameter packing table        *
C*      IADSTN         INTEGER           Number of additional stations  *
C*      MAXTIM         INTEGER           Max. # of times allowed in file*
C*      NHOURS         INTEGER           No. of hours before sys. time  *
C*                                                                      *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET           INTEGER          Return code                     *
C*                                         0 = Normal return            *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP      4/96                                            *
C* D. Kidwell/NCEP      1/98	New interface, cleaned up, merged LSDBLT*
C* D. Kidwell/NCEP      2/98	Changed minimum allowable bull. length  *
C* D. Kidwell/NCEP      3/98	Removed rpttim from calling sequences   *
C* D. Kidwell/NCEP      4/98	Added version number to log output      *
C* D. Kidwell/NCEP     10/98	Added intf mnemonics to call sequences  *
C* D. Kidwell/NCEP      4/00	Added text output                       *
C* D. Kidwell/NCEP      7/02	Added call to LS_AAXX                   *
C* A. Hardy/NCEP       12/03    Prepend bulletin time to text report	*
C* B. Yin/SAIC          3/04    Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'BRIDGE.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   curtim, gemfil, stntbl, prmfil
C* 
        INTEGER   	istarr(5), imnem(MMPARM), itype
        CHARACTER 	bulltn*(DCMXBF), sysdt*12, dattmp*12,
     +			errstr*80, cprms(MMPARM)*4, lsfrpt*(DCMXBF),
     +			iaaxx*5, cmdif*8, cdate*8, ctime*4, cdattm*15,
     +			parms(MMPARM)*4, tmprpt*(DCMXBF),
     +			rimnem(NRIMN)*8, cimnem(NCIMN)*8
        LOGICAL 	more, again, good, wrterr
C-----------------------------------------------------------------------
        iret = 0
C
C*	Write decoder version number to log.
C
	CALL DC_WLOG ( 2, 'DCLSFC', 8, '1.6', ierr )
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file.
C
	maxfil = 2
        iftype = 1
        CALL DC_FINT ( maxfil, iftype, prmfil, ierfnt )
C
C*	Set the pointers for the interface arrays.
C
	CALL LS_IFSP ( rimnem, cimnem, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL DC_WLOG ( 0, 'DCLSFC', -8, ' ', ierr )
	    RETURN
	END IF
C
C*	Initialize the GEMPAK parameter array and set up links to
C*	the interface mnemonics.
C
	CALL LS_INTF ( rimnem, cimnem, cprms, imnem, numprm, jret )
C
C*      Loop until timeout occurs.
C
        iperr = 0
C
        DO  WHILE ( iperr .eq. 0 )
C
C*          Get the next bulletin.
C
            CALL DC_GBUL ( bulltn, lenb, ifdtyp, iperr )
            IF ( iperr .eq. 0 ) THEN
C
C*		Parse the header info from the bulletin.
C
		more = .true.
		CALL DC_GHDR ( bulltn, lenb, seqnum, buhd, orign,
     +			       btime, bbb, nchrd, ierr )
		IF ( ierr .ne. 0) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
   		    CALL ST_UNPR ( bulltn(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCLSFC', 2, errstr, ier )
		    more = .false.
		END IF
	      ELSE
C
C*              Write timeout message to the log.
C
                CALL DC_WLOG ( 0, 'DC', iperr, ' ', ier )
		more = .false.
            END IF
	    IF ( more ) THEN
C
C*              Get the system time, and make a standard GEMPAK time
C*              from the "current" time.
C
		itype = 1
                CALL CSS_GTIM ( itype, sysdt, ier )
                IF ( curtim .eq. 'SYSTEM' ) THEN
                    dattmp = sysdt
                  ELSE
C
C*                  Convert the user's input time into the standard 
C*		    GEMPAK time.
C
                    CALL TI_STAN ( curtim, sysdt, dattmp, ier )
                END IF
C
C*              Convert GEMPAK time to integer time.
C
                CALL TI_CTOI ( dattmp, istarr, ier )
C
C*              Set receipt time significance to 0 and save it to
C*		lscmn.cmn.
C
                rctim (1) = 0.0
C
                DO i = 1, 5
                    rctim ( i + 1 ) = FLOAT ( istarr ( i ) )
                END DO
C
C*		Remove control characters from bulletin.
C
		CALL ST_UNPR ( bulltn, lenb, bulltn, lennew, ier )
		lenb = lennew
C
		IF ( lenb .ge. 40 ) THEN
C
C*      	    Some FAA bulletins contain a line with the 
C*		    block/station number followed by the string SM. 
C*		    Eliminate this line.           
C
        	    ipos = INDEX ( bulltn (20:40), ' SM ' )
                    IF ( ipos .gt. 0 ) THEN
C
C*      	        After the string '= ', the next 8 characters
C*		        will be eliminated.
C
                        again = .true.
        		i     = 0
        		j     = 0
C
 		        DO WHILE ( again )
 		            j = j + 1
              		    i = i + 1
           		    IF ( bulltn (j:j+1) .eq. '= ' ) THEN
             		        bulltn (i:i+1) = '= '
              		        i = i + 1
              			j = j + 10
           		      ELSE IF ( bulltn (j:j+2) .eq. '== ') THEN
              			bulltn (i:i+1) = '= '
              			i = i + 1
              			j = j + 11
           		      ELSE
              			bulltn (i:i) = bulltn (j:j)
           		    END IF
C
           		    IF ( j .ge. lenb ) THEN
              			lenb  = i
              			again = .false.
           		    END IF
        		END DO
		    END IF
C
C*      	    Check that bulletin contains land synoptic reports.
C
        	    CALL LS_BTYP ( lenb, bulltn, jpos, iaaxx, jret )
C
        	    IF ( ibrtyp .eq. 1 ) THEN
C
C*      		Write bulletin header data to the decoder log.
C
			logmsg = seqnum // buhd // orign // btime // bbb
        		CALL DC_WLOG ( 4, 'DCLSFC', 7, logmsg(1:36),ier)
		      ELSE
			more = .false.
		    END IF
		  ELSE
C
C*		    Reject bulletins less than 40 characters long.
C
		    more = .false.
		END IF
	    END IF
C
C*          Loop through reports.
C
            DO WHILE ( more )
                good = .true.
C
C*              Get next report from bulletin.
C
                CALL LS_GRPT ( lenb, bulltn, jpos, lszrpt, lsfrpt, jret)
C
		IF ( jret .eq. 3 ) THEN
C
C*		    Found another set of AAXX YYGGi(w) groups in
C*		    bulletin.
C
		    iaaxx = bulltn ( jpos-5:jpos-1 )
		    CALL LS_AAXX ( iaaxx, istarr, ier )
		END IF
C
                IF ( jret .eq. 2 ) THEN
C
C*                  There are no more reports in bulletin.
C
                    more = .false.
		    good = .false.
		  ELSE IF ( jret .ne. 0 ) THEN
		    good = .false.
		  ELSE
C
C*                  Initialize report parameters to missing.
C
                    CALL LS_INIT ( jret )
C                    
C*                  Check length of groups in report.
C
                    CALL LS_CKRP ( lsfrpt, 6, lszrpt, jret )
                    IF ( jret .ne. 0 ) THEN
                        good = .false.
                      ELSE
C
C*                      Decode section 0 of report.
C
                        CALL LS_AST0 ( lszrpt, lsfrpt, iaaxx, istarr,
     +				       ipt, jret)
C
                        IF ( jret .ne. 0 ) good = .false.
                    END IF
		END IF
C
                IF ( good ) THEN
		    lsz = MIN ( 50, lszrpt )
		    wrterr = .false.
C
C*                  Compute difference in minutes between
C*                  observation and system times.
C
                    CALL TI_MDIF ( irptdt, istarr, imdif, ier1 )
                    itime = irptdt ( 4 ) * 100 + irptdt ( 5 )
C
C*                  Report time for GEMPAK must not be more than
C*                  nhours before the system time, and not more than
C*                  60 minutes after.
C
                    IF ( ( ier1 .ne. 0 ) .or. ( imdif .gt. 60 ) .or.
     +                   ( imdif. lt. ((-60)*nhours) ) ) THEN
                        good   = .false.
			wrterr = .true.
                        errstr = buhd // orign // btime
                        CALL DC_WLOG ( 2, 'DCLSFC', 3, errstr, ier )
                        CALL ST_INCH ( imdif, cmdif, ier )
                        idate  = irptdt ( 1 ) * 10000 + 
     +		                 irptdt ( 2 ) * 100 + irptdt ( 3 )
                        CALL ST_INCH ( idate, cdate, ier )
                        CALL ST_INCH ( itime, ctime, ier )
                        cdattm = cdate( 3:) // '/' // ctime
                        errstr = cdattm // dattmp // cmdif
                        CALL DC_WLOG ( 2, 'DCLSFC', 4, errstr, ier )
                    END IF
C
                    IF ( good ) THEN
			
C
C*		        Open the GEMPAK file.
C
			CALL LS_GEMP ( gemfil, stntbl, iadstn, maxtim, 
     +				       lunf, nparm, parms, jret )
			IF ( jret .ne. 0 ) THEN
			    good = .false.
			    IF ( jret .eq. -1 .or. jret .eq. -2 ) 
     +                           wrterr = .true.
			END IF
		    END IF
		    IF ( good ) THEN
C
C*                      Decode the bulletin report and write the
C*                      data to the output file.
C
                        CALL LS_DCD1 ( lszrpt, lsfrpt, ipt, jret )
C
C*                      Output decoded report.
C 
                        IF ( jret .eq. 0 ) THEN
		            CALL DC_WLOG ( 4, 'LS', 6, lsfrpt (1:lsz), 
     +  				   ierr )
C
C*                          Write decoded values to the decoder log.
C
C                           CALL LS_IFPT ( lszrpt, lsfrpt, rimnem, 
C    +					   cimnem, ier )
C
C*                          Write report to GEMPAK file.
C
                            CALL LS_WGEM ( lunf, nparm, parms,
     +					   cprms, imnem, numprm, jret )
			    IF ( jret .ne. 0 ) THEN
				wrterr = .true.
			      ELSE
C
C*			        Prepend bulletin time to report.
C
                                tmprpt = lsfrpt (:lszrpt)
                                CALL ST_LSTR ( btime, lenbtm, ier )
				lsfrpt = btime (:lenbtm) // ' '
     +                                   // tmprpt(:lszrpt)
                                CALL ST_LSTR ( lsfrpt, lszrpt, ier )
C
C*			        Write the text to the GEMPAK file.
C
			        CALL SF_WSTR ( lunf, itime, 
     +					       lsfrpt ( :lszrpt ), ier )
			    END IF
			  ELSE
			    wrterr = .true.
                        END IF
                    END IF
		    If ( wrterr ) CALL DC_WLOG ( 4, 'LS', -2, 
     +			          lsfrpt (1:lsz), ier)
                END IF
            END DO
        END DO
C
C*      Close the land surface files.
C
        CALL DC_FCLS ( ier )
C*
        RETURN
        END
