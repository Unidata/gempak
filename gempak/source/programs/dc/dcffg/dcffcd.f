	SUBROUTINE DCFFCD ( curtim, gemfil, prmfil, stntbl, iadstn,
     +			    maxtim, nhours, txtflg, iret )
C************************************************************************
C* DCFFCD								*
C*									*
C* This routine will decode Flash Flood Guidance bulletins and write 	*
C* the data to a GEMPAK surface file.					*
C*									*
C* DCFFCD ( CURTIM, GEMFIL, PRMFIL, STNTBL, IADSTN, MAXTIM,		*
C*	    NHOURS, TXTFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*					  to decode			*
C*	TXTFLG		LOGICAL		Flag to save undecoded text	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = no .B or .END found	*
C*					 -8 = no stations decoded	*
C*					 -9 = invalid issuance time	*
C*									*
C**									*
C* Log:									*
C* L. Sager/NCEP	 5/96	Copied from DCADCD                      *
C* S. Jacobs/NCEP	 7/96	Changed calling sequence; Updated calls	*
C* S. Chiswell/Unidat	10/96	Added bbb to declarations		*
C* K. Tyle/GSC		 1/97	Changed DC_WLOG calls; eliminated call	*
C* 				to IN_BDTA				*
C* K. Tyle/GSC		 3/97	Change itep to iflsrc; initialize 	*
C*				iperr; change error processing;		*
C*				use single decoding subroutine FF_DECD	*
C* K. Tyle/GSC		 4/97	Added originating station KSAC; pass	*
C*				oristn to FF_DECD			*
C* K. Tyle/GSC		 5/97	Decode AFOS bulletins; use report time;	*
C*				reorganized subroutine			*
C* D. Kidwell/NCEP      12/97   Removed check on valid originating stn  *
C* D. Kidwell/NCEP       1/98   Added code to save most recent time     *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* D. Kidwell/NCEP      11/05   Improved error checks                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
	LOGICAL		txtflg
C*
	CHARACTER	parms(MMPARM)*4
	CHARACTER	wmohdr*8, pilhdr*12
C*
	CHARACTER	bultin*(DCMXBF), dattim*15, bbb*8,
     +			seqnum*4,  oristn*8, btime*12, sysdt*12,
     +			dattmp*12, filnam*132, errstr*80, cmdif*8,
     +			rpttim*12, datrpt*12, rtime*12, datcur*12
	CHARACTER*20    cntyid(300)

	REAL		rdata (MMPARM)
	INTEGER		istarr (5), irtarr (5), istart (300), irptar(5)
	INTEGER		itype
 	REAL    	ff01 (300), ff03 (300), ff06(300), ff12(300),
     +			ff24(300)

	LOGICAL		good, addstn, cirflg, datflg, newbul
C*
C*	The order of the following list of parameters MUST be
C*	maintained in the list of data elements near the end of
C*	this routine.
C*
	PARAMETER	( NUMPRM = 5 )
	PARAMETER	( NUMEXT = MMPARM - NUMPRM )
	CHARACTER	cprms (MMPARM)*4
	INTEGER		iprms (MMPARM)
	DATA		cprms / 'FF01', 'FF03', 'FF06', 'FF12',
     +				'FF24',
     +				NUMEXT * ' ' /
C------------------------------------------------------------------------
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file to 1 for surface.
C
	maxfil = 2
	iftype = 1
	CALL DC_FINT  ( maxfil, iftype, prmfil, ier )
	addstn = .true.
	cirflg = .false.
	iret   = 0
C
C*	Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE  ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL  ( bultin, lenbul, ifdtyp, iperr )
	    IF  ( iperr .eq. 0 )  THEN
C
C*		Parse the header info from the bulletin.
C
		good   = .true.
		newbul = .true.
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR  ( bultin, lenbul, seqnum, wmohdr,
     +				    oristn, btime, bbb, nchar, ierr )
		    errstr = 'Receiving ' // seqnum // wmohdr //
     +			     oristn // btime
		    CALL DC_WLOG  ( 4, 'DCFFG', 2, errstr, ier )
		  ELSE
		    CALL DC_GPIL  ( bultin, lenbul, pilhdr, wmohdr,
     +				    oristn, btime, nchar, ierr )
		    bbb = ' '
		    errstr = 'Receiving PIL ' // pilhdr // wmohdr //
     +			     oristn // btime
		    
		    CALL DC_WLOG  ( 4, 'DCFFG', 2, errstr, ier )
		END IF
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG  ( 1, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR  ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG  ( 1, 'DCFFG', 2, errstr, ier )
		    good = .false.
		END IF
C
 	        IF  ( good )  THEN
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
C
	 	    itype = 1
		    CALL CSS_GTIM  ( itype, sysdt, ier )
C
		    IF  ( curtim .eq. 'SYSTEM' )  THEN
		        dattmp = sysdt
		      ELSE
		        CALL TI_STAN  ( curtim, sysdt, dattmp, ier )
		    END IF
 		    CALL TI_CTOI  ( dattmp, istarr, ier )
C
C*		    Get bulletin time.
C
		    CALL FF_RTIM  ( istarr, btime, irtarr, dattim,
     +				    ier1 )
		    ihhmm = irtarr(4) * 100
		    IF  ( ier1 .ne. 0 )  THEN
			CALL DC_WLOG  ( 1, 'DCFFG', ier1, btime, ier )
		    END IF
C
C*		    Compute difference between bulletin and
C*		    system times.
C
		    CALL TI_MDIF  ( irtarr, istarr, imdif, ier2 )
C
C*		    Check that the time is within NHOURS before
C*		    the system time.
C
		    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +			  ( imdif .gt. 60 ) .or.
     +			  ( imdif .lt. ((-60)*nhours) ) ) THEN
			good = .false.
C
C*			Write an error message if the time is
C*			invalid.
C
			errstr = wmohdr // oristn // btime
			CALL DC_WLOG  ( 2, 'DCFFG', 3, errstr, ier )
			CALL ST_INCH  ( imdif, cmdif, ier )
			errstr = dattim // dattmp // cmdif
			CALL DC_WLOG  ( 1, 'DCFFG', 4, errstr, ier )
		    END IF
C
C*		    Locate the beginning of each line in this bulletin,
C*		    and get the report time.
C		    
		    CALL FF_GLIN ( bultin, lenbul, oristn, istart,
     +				   numlin, rpttim, iret )
		    IF ( iret .eq. 0 ) THEN
		   	rtime = rpttim(5:6)//rpttim(8:11)
			CALL FF_RTIM  ( istarr, rtime, irptar, datrpt,
     +				        ier1 )
C
C*			Compute difference between report time
C*			and bulletin time.
C
			IF ( ier1 .eq. 0 ) THEN
		    	    ihhmm = irptar(4) * 100
			    CALL TI_MDIF ( irtarr, irptar, nmin, ier )
C
C*			    Check that report time is within 3 hours of 
C*			    bulletin time.
C
			    IF ( ABS ( nmin ) .gt. 180 ) THEN
				good = .false.
				iret = -9
				errstr = dattim // ' ' // datrpt
				CALL DC_WLOG (2,'DCFFG', -9,errstr,ier)
			    END IF
			  ELSE
			    CALL DC_WLOG ( 2, 'DCFFG', ier1, rtime, ier)
			    good = .false.
			END IF
		      ELSE		
C
C*			Use bulletin time if no report time.
C
			IF ( iret .gt. 0 ) datrpt = dattim
C
C*			Reject if no .B / .END lines found.
C
			IF ( iret .lt. 0 ) good = .false.
			CALL DC_WLOG ( 2, 'DCFFG', iret, ' ' , ier )
		    END IF
		END IF
C
C*		Open the output file.
C
		IF  ( good )  THEN
C
C*		    Make a file name from the template and the time.
C*		    Open the file as part of the open file list.
C
		    CALL FL_MNAM  ( datrpt, gemfil, filnam, ier )
		    iflsrc = MFUNKN
		    CALL DC_FCYL  ( filnam, iflsrc, stntbl, iadstn,
     +				    maxtim, lunf, nparm, parms,
     +				    ierr )
C
C*		    Check that the file was opened properly.
C
		    IF  ( ierr .ne. 0 )  THEN
C
C*			If not, write an error to the decoder
C*		        log file.
C
			CALL DC_WLOG  ( 0, 'SF', ierr,
     +				        filnam, ier )
			good = .false.
		      ELSE
C
C*			Check for the parameters in the list.
C
			DO  k = 1, NUMPRM
			    CALL ST_FIND  ( cprms(k), parms, nparm,
     +					    iprms(k), ier )
			    IF  ( iprms (k) .eq. 0 )  THEN
				iprms (k) = nparm + 1
				CALL DC_WLOG  ( 1, 'DCFFG', -3,
     +						cprms(k), ier )
			    END IF
			END DO
C
C*			Create a unique "current" time.
C
			datcur = datrpt
			datcur ( 8:11 ) = '1212'
		    END IF
		END IF
C
C*		Decode the data, set the station and time into the
C*		GEMPAK output file and output the data.
C
		IF ( good ) THEN
C
C*		    Set the output arrays to missing.
C
		    DO k = 1, 300
			cntyid (k) = ' '
			ff01   (k) = RMISSD
			ff03   (k) = RMISSD
			ff06   (k) = RMISSD
			ff12   (k) = RMISSD
			ff24   (k) = RMISSD
		    END DO
C
C*		    Decode the bulletin.
C
		    CALL FF_DECD ( bultin, oristn, istart, numlin,
     +			       	   cntyid, ff01, ff03, ff06, ff12,
     +				   ff24, irec, iret )
C
		    IF ( iret .ge. 0 ) THEN
			DO k = 1, irec
			    good = .true.
			    CALL RA_TMST ( lunf, datrpt, cntyid(k), 
     +				    	  addstn, cirflg, datflg, iret )
C
C*			    Check for an error.
C
			    IF  ( iret .ne. 0 )  THEN
				good = .false.
			        IF ( iret .eq. -4 ) errstr = datrpt
			        IF ( iret .eq. -5 ) errstr = cntyid(k)
			        CALL DC_WLOG  ( 1, 'RA', iret,
     +						errstr, ier )
C
C*			        If the data has already been decoded, 
C*			        do not decode again.
C
			      ELSE IF  ( datflg ) THEN
				good = .false.
			    END IF
C
C*			    Move data into output array.
C*			    The following list of data elements MUST
C*			    be in the same order as the list of
C*			    parameters in the DATA statement at the
C*			    beginning of this routine.
C
			    IF ( good ) THEN
				rdata (1) = ff01 (k)
				rdata (2) = ff03 (k)
				rdata (3) = ff06 (k)
				rdata (4) = ff12 (k)
				rdata (5) = ff24 (k)
C
C*				Write the data to the output file.
C
				IF ( newbul ) THEN
				    IF ( ifdtyp .eq. 0 ) THEN
					errstr = 'Writing ' // seqnum //
     +					     wmohdr // oristn // btime
				      ELSE
					errstr = 'Writing ' // pilhdr //
     +					     wmohdr // oristn // btime
				    END IF
				    CALL DC_WLOG  ( 4, 'DCFFG', 2, 
     +						    errstr, ier )
				    newbul = .false.
				END IF
				CALL SF_WDAT (lunf,ihhmm,rdata,iret)
C
C*				Write data as "current" time.
C
			        CALL RA_TMST ( lunf, datcur, cntyid(k), 
     +				    	  addstn, cirflg, datflg, iret )
C
C*			        Check for an error.
C
			        IF  ( iret .ne. 0 )  THEN
				    good = .false.
			            IF (iret .eq. -4) errstr = datcur
			            IF (iret .eq. -5) errstr = cntyid(k)
			            CALL DC_WLOG  ( 1, 'RA', iret,
     +						    errstr, ier )
				  ELSE
				    CALL SF_WDAT (lunf,ihhmm,rdata,iret)
			        END IF
			    END IF
			END DO
		    END IF
	        END IF
	      ELSE
C
C*		Write an error to the decoder log file.
C
		CALL DC_WLOG ( 0, 'DC', iperr, ' ', ier )
	    END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
