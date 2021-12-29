	SUBROUTINE GG_EDR ( dattim, htinc,  htcolrs, numc, tlimit,
     +                      evinc,  ecolrs, esymb1, esymb2, esymbsz1,
     +                      esymbsz2, enumc, aoa180fl, tracksfl, iret )
C*
C************************************************************************
C* GG_EDR                                                               *
C*                                                                      *
C* This subroutine sets up the times and attributes for plotting        *
C* EDR tracking data.                                                   *
C*                                                                      *
C* GG_EDR (DATTIM, HTINC, HTCOLRS, NUMC, TLIMIT, EVINC, ECOLRS,         *
C*          ENUMC, ABV180FL, IRET )                                     *
C*                                                                      *
C* Input parameters:                                                    *
C*	DATTIM		CHAR*		Ending time for ASDI data       *
C*	HTINC(*)	INTEGER 	HT increments in Hundreds feet  *
C*	HTCOLRS(*)	INTEGER		Color for each Height           *
C*	NUMC		INTEGER		Number of Ht colors             *
C*	TLIMIT   	INTEGER		Time Limit to plot EDR          *
C*	EVINC(*)        REAL            EDR VALUE LIMITs                *
C*      ECOLRS(*)       INTEGER         Color for each EDR VALUE        *
C*      ESYMB1(*)       INTEGER         Symbols BLO FL180               *
C*      ESYMB2(*)       INTEGER         Symbols AOA FL180               *
C*      ESYMBSZ1(*)     REAL            Size for each EDR VALUE BLO 180 *
C*      ESYMBSZ2(*)     REAL            Size for EDR Values AOA FL180   *
C*	ENUMC           INTEGER         Number of Colors                *
C*      AOA180FL        INTEGER         FLAG to Plot 180 Feet+ Only     *
C*      TRACKSFL        INTEGER         FLAG to Plot EDR with tracks    *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* L. Hinson/AWC        09/12                                           *
C* L. Hinson/AWC        10/18 Updated to add tracks flag                *
C************************************************************************ 
	INCLUDE 'GEMPRM.PRM'
        
	CHARACTER*(*) dattim
	INTEGER       htinc (*), htcolrs (*)
	INTEGER       numc, tlimit
        REAL          evinc (*)
        INTEGER       ecolrs (*)
        INTEGER       esymb1 (*)
        INTEGER       esymb2 (*)
        REAL          esymbsz1 (*)
        REAL          esymbsz2 (*)
        INTEGER       enumc
        LOGICAL       aoa180fl
        LOGICAL       tracksfl
	PARAMETER	( NUMSRC = 1 )
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +         		tfile*128, stime*20, flstrt*160, datfil*20,
     +			datgrp (LLCLEV)*20
	CHARACTER*(MXFLSZ) filnam, files (MXNMFL), alias (NUMSRC)
	INTEGER		itarr(5), jtarr(5), htinc2 (LLCLEV),
     +			htcolrs2 (LLCLEV), itype, numclr, enumclr,
     +                  ecolrs2 (LLCLEV)
	REAL		htincr(LLCLEV), evinc2(LLCLEV)
        INTEGER         wifl
	LOGICAL		done
C*
	DATA		alias / 'EDR' /
C---------------------------------------------------------
C*	 
	iret = 0
C
        numclr = numc
	IF ( numclr .gt. 0 ) THEN
 	    DO ii = 1, numclr
		htinc2 ( ii ) = htinc ( ii )
       		htcolrs2 ( ii ) = htcolrs ( ii )
C*              Create Real array of heights fo use in GG_CBAR (below).
		htincr ( ii ) = htinc ( ii ) * 1.0
	    END DO
	ELSE
C
C*	    No times or colors were specified.  Set defaults.
C
	    numclr = 1
	    htinc2 ( 1 ) = 250
	    htincr ( 1 ) = 250.
	    htcolrs2 ( 1 ) = 19
	END IF
	htinc2 (numclr+1) = htincr (numclr)
	htcolrs2 (numclr+1) = 0
        
        enumclr = enumc
        
        IF ( enumclr .gt. 0 ) THEN
          DO ii = 1, enumclr
            evinc2 (ii) = evinc (ii)
            ecolrs2 (ii) = ecolrs (ii)
          END DO
        ELSE
          enumclr = 1
          evinc2 ( 1 ) = 0.15
          ecolrs ( 1 ) = 24
        END IF
        
        evinc2 (enumclr+1) = evinc (enumc)
        ecolrs2 (enumclr+1) = 0
        
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
C*	Calculate how far to go back by using the time limit
C
C        
        minuts = tlimit
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
       
C
        nhts = numclr + 1
        
	DO ks = 1, NUMSRC
C
C*	        Scan the directory for all of the EDR data files.
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
C*		       Read and plot the EDR data.
C
                       CALL GG_EDRP ( lunf, dattm2, datfil, datgrp, 
     +                                nhts, htinc2, htcolrs2, tlimit,
     +                                enumclr, evinc2, esymb1, esymb2,
     +                                esymbsz1, esymbsz2, ecolrs2, 
     +                                aoa180fl, tracksfl, ier )
                    END IF		      
C
	        END IF
	END DO
C
C*	Draw the color bar.
C

        CALL GG_CBAR ( '1/V/LR/.995;.05/.5;.01/-1',
     +			numclr, htincr, htcolrs2, ier )
        CALL GG_CBAR ( '1/V/LR/.95;.05/.5;.01/-1',
     +			enumclr, evinc2, ecolrs2, ier )
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	RETURN
	END
