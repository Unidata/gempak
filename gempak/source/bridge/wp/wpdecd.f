	SUBROUTINE WP_DECD ( bultin, lenbul, isstim, lunf, iret )
C************************************************************************
C* WP_DECD 								*
C*									*
C* This subroutine decodes a single WWUS60 watch corner point report.   *
C*                                                                      *
C* WP_DECD ( BULTIN, LENBUL, LUNF, IRET )				*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		Watch or status bulletin        *
C*	LENBUL		INTEGER		Length of bulletin              *
C*	ISSTIM		CHAR*		Issue time from bulletin header	*
C*	LUNF		INTEGER		Logical unit no. for output file*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, isstim
C*
	PARAMETER       ( MAXPTS = 50 )
	CHARACTER	strtim*20, endtim*20, wnum*4, wtype*2
	REAL   		alat (MAXPTS), alon (MAXPTS)
	LOGICAL		done, more, good
	CHARACTER	work*(DCMXBF), carr (4)*10
	INTEGER  	idtarr (5)
C------------------------------------------------------------------------
	iret = 0
C
     		work = bultin(:lenbul)
     		lenw = lenbul
C
C*	Ensure upper case
C
	CALL ST_LCUC ( work, work, ier )
C
C*	Process WCP report
C
	more = .true.
	done = .false.
	ibpnt = 0
	lenwv = lenw
	DO WHILE ( .not. done .and. ibpnt .lt. lenwv )
	    good = .true.
C
C*	    Determine if there are watches or none active.
C
	    ibpsv = ibpnt
	    ibpnt = INDEX ( work (ibpsv:lenw), 'SEVR ' )
	    IF ( ibpnt .eq. 0 ) THEN
C
C*		Check for "FILE CREATED" in a no active watches report 
C
		ibpnt = INDEX (work (:lenw), 'FILE CREATED' )
		IF ( ibpnt .eq. 0 ) THEN
		    good = .false.
		  ELSE
		    nmpt = 0
C
C*		    Process no active watches report
C
		    CALL WP_NOWT ( work (ibpnt:lenw), lenw, wtype, strtim,
     +				   endtim, wnum, iret )
		    IF ( iret .ne. 0 ) good = .false.
		END IF
		done = .true.
	      ELSE
C
C*		Decode Watch Corner Point (WCP) Report
C*
C*		First, decode SEVR (watch type) line having start
C*		time, end time, and type.
C
		isgb = ibpnt + ibpsv + 4
		isge = isgb + 24
		CALL ST_CLST ( work (isgb:isge), ' ', ' ', 4, carr,
     +			numc, ier )
		CALL ST_LSTR ( carr(1), lndate, ier )
		IF ( lndate .ne. 6 ) carr(1) = isstim(1:6)
		strtim =  carr(1)(1:6) // '/' // carr(2)(:4)	
		CALL ST_NUMB ( carr(2)(:4), istart, ier )
		IF ( ier .ne. 0 ) THEN
		    good = .false.
		    ibpnt = ibpnt + ibpsv + 4
		    CALL DC_WLOG ( 2, 'DCWCP', -3,
     +				work (isgb:isgb+60), ier )
		END IF
		CALL ST_NUMB ( carr(4)(:4), iend, ier )
		IF ( ier .ne. 0 ) THEN
		    good = .false.
		    ibpnt = ibpnt + ibpsv + 4
		    CALL DC_WLOG ( 2, 'DCWCP', -3,
     +				work (isgb:isgb+60), ier )
		END IF
		endtim = carr(1)(1:6) // '/' // carr(4)(:4)		
	       IF ( good ) THEN
		IF ( istart .gt. iend ) THEN
C
C*		    Convert endtim to the next day
C
		    CALL TI_CTOI ( endtim, idtarr, ier )
		    CALL TI_ADDD ( idtarr, idtarr, ier )
		    CALL TI_ITOC ( idtarr, endtim, ier )
		END IF
		IF ( carr(3)(1:2) .eq. 'WT' ) THEN
		    wtype = "TN"
		  ELSE IF ( carr(3)(1:2) .eq. 'WS' ) THEN
		    wtype = "TS"
		  ELSE
		    good = .false.
		    ibpnt = ibpnt + ibpsv + 4
		    CALL DC_WLOG ( 2, 'DCWCP', -5,
     +				work (isgb:isgb+60), ier)
		END IF
	       END IF
		IF ( good ) THEN
		    wnum = carr(3)(3:6)
C
C*		    Invoke WP_LTLN to decode the lat/lon line(s). 
C*		    Use CHLF and ; to find beginning and end of
C*		    lat long string
C
		    illb = INDEX ( work (isgb:lenw), CHLF )
		    ille = INDEX ( work (isgb:lenw), ';' )
		    IF ( illb .ne. 0 .and. ille .ne. 0 .and.
     +			    ille .gt. illb ) THEN
		        CALL WP_LTLN ( work (isgb+illb:isgb+ille),
     +			    alat, alon, nmpt, ierll )
			IF ( ierll .ne. 0 ) THEN
			    good = .false.
			    CALL DC_WLOG ( 2, 'DCWCP', ierll,
     +				    work (isgb+illb:isgb+ille), ier )
			END IF
		        ibpnt = isgb + ille + 1
		      ELSE
			good = .false.
			CALL DC_WLOG ( 2, 'DCWCP', -6,
     +				    work (isgb:isgb+60), ier )
			IF ( ille .eq. 0 ) THEN
			   done = .true.
			 ELSE
			   ibpnt = isgb + ille + 1
			END IF
		    END IF
		END IF
	    END IF

	    IF ( good ) THEN
C
C*		Write WCP info to ASCII file.
C
		CALL WP_OUT ( lunf, wtype, isstim, strtim, endtim,
     +			wnum, nmpt, alat, alon, iret )
	    END IF
	END DO
C*
	RETURN
	END
