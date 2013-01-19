	SUBROUTINE GG_ASDI (dattim, ktminc, kcolrs, numc, 
     +                      tlimit, mode, depdest, sites, iret)
C*
C************************************************************************
C* GG_ASDI                                                              *
C*                                                                      *
C* This subroutine sets up the times and attributes for plotting        *
C* ASDI tracking data.                                                  *
C*                                                                      *
C* GG_ASDI (DATTIM, KTMINC, KCOLRS, NUMC, TLIMIT, MODE, DEPDEST,        *
C*          SITES, IRET )                                               *
C*                                                                      *
C* Input parameters:                                                    *
C*	DATTIM		CHAR*		Ending time for ASDI data       *
C*	KTMINC(*)	INTEGER 	Time increments in minutes      *
C*	KCOLRS(*)	INTEGER		Color for each time increment   *
C*	NUMC		INTEGER		Number of colors                *
C*	TLIMIT   	INTEGER		Time Limit to plot ASDI DATA    *
C*	MODE            CHAR*           T for Time, H for Heights       *
C*      DEPDEST         CHAR*           D for Departures, A for Arrivals*
C*                                      B for Both                      *                        
C*	SITES           CHAR*           The SITE ID, a semi-colon       *
C*                                      separated list of SITE IDs, or  *
C*                                      ALL to plot ALL sites.          *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* L. Hinson/AWC        05/12                                           *
C************************************************************************ 
	INCLUDE 'GEMPRM.PRM'
        
	CHARACTER*(*) dattim
	INTEGER       ktminc (*), kcolrs (*)
	INTEGER  numc, tlimit
        CHARACTER*(*) mode
        CHARACTER*(*) depdest
	CHARACTER*(*) sites
	PARAMETER	( NUMSRC = 1 )
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +         		tfile*128, stime*20, flstrt*160, datfil*20,
     +			datgrp (LLCLEV)*20
	CHARACTER*(MXFLSZ) filnam, files (MXNMFL), alias (NUMSRC)
	INTEGER		itarr(5), jtarr(5), itminc (LLCLEV),
     +			icolrs (LLCLEV), itype
	REAL		ftminc(LLCLEV), wifl
	LOGICAL		done
C*
	DATA		alias / 'ASDI' /
C---------------------------------------------------------
C*	 
	iret = 0
C
	numclr = numc
	IF ( numclr .gt. 0 ) THEN
 	    DO ii = 1, numclr
		itminc ( ii ) = ktminc ( ii )
		ftminc ( ii ) = ktminc ( ii )
		icolrs ( ii ) = kcolrs ( ii )
	    END DO
	  ELSE
C
C*	    No times or colors were specified.  Set defaults.
C
	    numclr = 1
	    itminc ( 1 ) = 5
	    ftminc ( 1 ) = 5.
	    icolrs ( 1 ) = 2
	END IF
	ftminc (numclr+1) = ftminc (numclr)
	icolrs (numclr+1) = 0
        nexp   = MXNMFL
	iorder = 1
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( dattim .eq. 'LAST' ) THEN
	    CALL CSS_GTIM ( itype, dattm2, ier )
	  ELSE IF ( dattim .eq. 'ALL' ) THEN
	    RETURN
	  ELSE
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    	iret = ier
	    	return
	    ENDIF	    	
	END IF
C
C*	Calculate how far to go back by using the last time increment.
C
C        
        minuts = itminc ( numclr )
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
        ntimes = numclr + 1
        
        IF (mode .eq. 'T') THEN
C*	Get the break times for changing the marker color.
C*	Subtract each increment from the frame time and add it to 
C*	the list with the earliest time first.
C
	  datgrp ( ntimes ) = dattm2
	  CALL TI_CTOI ( dattm2, itarr, ier )
	  indx = 1
	  DO ii = numclr, 1, -1
	      CALL TI_SUBM ( itarr, itminc(ii), jtarr, ier )
	      CALL TI_ITOC ( jtarr, datgrp(indx), ier )
	      indx = indx + 1
	  END DO
        END IF
        CALL GQCOLR ( jcolr, ier )	
	DO ks = 1, NUMSRC
C
C*	        Scan the directory for all of the ASDI data files.
C
		CALL ST_NULL ( alias ( ks ), alias ( ks ), na, ier )
		path  = ' '
		templ = ' '
		CALL CTB_DTGET ( alias ( ks ), path, templ,  
     +				 ic, is, if, ir, ii, ion, ihb, mnb,
     +				 iha, mna, mstrct, idtmch, ier )
		CALL ST_RNUL ( path, path, lens, ier )
		CALL ST_RNUL ( templ, templ, lens, ier )
	        CALL ST_LSTR ( path, lenp, ier )
	        CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, 
     +                         ier )
		IF ( ier .eq. 0 ) THEN
C
C*		    This data source exists.  Make the file name for the
C*		    last file requested.
C
	            CALL FL_MNAM ( dattm2, templ, filnam, ier )
C
C*	            Find the earliest file to start searching.
C
	            CALL FL_MNAM ( stime, templ, flstrt, ier )

C
C*
C                   Find the nearest file to the time requested, and
C                   plot the data... Nearest file must be the one at
C                   or older to the time requested.
                    done = .false.
		    ifl = 1
		    tfile = ''
		    DO WHILE (( ifl .le. nfile) .and. ( .not. done ) )
		      IF ( files (ifl) .gt. filnam ) THEN
		        done = .true.
		      ELSE
		        IF (files(ifl) .ge. flstrt ) THEN
			  tfile = path(:lenp) // '/' // files(ifl)
			  wifl = ifl
			END IF
	              END IF
		      ifl = ifl + 1
		    END DO
		    IF (tfile .ne. '') THEN
                       CALL FL_SOPN ( tfile, lunf, ier )
C
C*		       Get the time for this data from the file name.
C
		       CALL FL_MDAT ( files ( wifl ), templ, 
     +				  	       dattm2, datfil, ier )
C
C*		       Read and plot the ASDI data.
C
                       CALL GG_ASDIP ( lunf, dattm2, datfil,
     +					       datgrp, ntimes, itminc, 
     +                                         icolrs, tlimit, mode,
     +                                         depdest, sites, ier )		      
                    END IF		      
C
	        END IF
	END DO
C
C*	Draw the color bar.
C
        IF ( depdest .eq. "B" .or. depdest .eq. "D") THEN
          CALL GG_CBAR ( '1/V/LR/.995;.05/.5;.01/-1',
     +			numclr, ftminc, icolrs, ier )
        END IF
        IF ( depdest .eq. "A" ) THEN
          CALL GG_CBAR ( '1/V/LR/.96;.05/.5;.01/-1',
     +			numclr, ftminc, icolrs, ier )
        END IF
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	RETURN
	END
