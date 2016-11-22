	PROGRAM  GDINFO
C************************************************************************
C* PROGRAM GDINFO							*
C*									*
C* This program prints information about GEMPAK grid files.		*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	4/85						*
C* M. desJardins/GSFC	5/86	Changed IP_OUTT to IN_OUTT		*
C* I. Graffman/RDS	7/88	Changed to all grids listed		*
C* K. Brill/NMC         9/90    Change GDLIST to LSTALL			*
C* J. Whistler/SSAI	5/91	Changed output*24 to output*48		*
C* K. Brill/NMC		2/92	Use LLNNAV, LLNANL			*
C* S. Jacobs/EAI	3/93	Changed call to GR_LIST			*
C* L. Williams/EAI      3/94    Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	7/94	Removed call to GDIUPD			*
C* D. Keiser/GSC	8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDIGEN		*
C* S. Maxwell/GSC	10/96	Changed GR_LIST, added parameters for 	*
C*				matching				*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D.W.Plummer/NCEP	 4/00	Changes for multiple fcst hour files	    *
C* D.W.Plummer/NCEP	 5/00	Re-code to remove need for GPLT		        *
C* D.W.Plummer/NCEP	 5/00	Add cycle processing; EXIT for mult fhr     *
C* S. Jacobs/NCEP        6/00   Change gdclst size from 256 to 12*LLMXGT*
C* B.E.McDonald/NCEP            Change cycles from 100 to LLMXGT        *
C*                              Change ST_CLST to ST_CLSL               *
C* T. Lee/GSC		11/00	Checked blank file			                *
C* S. Jacobs/NCEP	 1/01	Added to check for blank file name	        *
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		        *
C* A. Hardy/NCEP	11/04   Added calls to ST_RNUL			            *
C* m.gamazaychikov/SAIC	12/04	Added ionoff flag to CTB_DTGET CS	    *
C* R. Tian/SAIC          3/05   Changes for time/file mngmnt            *
C* R. Tian/SAIC          4/05   Fixed a bug for dual time		        *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* S. Jacobs/NCEP	10/13	Added check for reading data from 	        *
C*				the AWIPS database			                            *
C* S. Gilbert/NCEP   9/15   Added cycle to filename for AWIPSDB         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		lstall
	CHARACTER	gdfile*(LLMXLN), output*(LLMXLN),
     +			gdattim*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), gfunc*(LLMXLN),
     +			gdfileo*(LLMXLN)
C*
	CHARACTER	devs(4)*1, ans*1, filnam*(MXFLSZ)
	CHARACTER	trange*36, tmfst*20, tmlst*20, tmpdtm*20
	CHARACTER	farr(2)*(LLMXLN), timfnd(LLMXGT)*256
	CHARACTER	path*(MXFLSZ), tmplt*64, vparm*(LLMXLN)
	CHARACTER	prmlst(MMPARM)*(LLMXLN)
	REAL		rlevel (LLMXLV,2)
	INTEGER		luns (4)
	LOGICAL		respnd, done, proces, pause, title, exist
	LOGICAL		awpflg
	REAL		anl (LLNANL), rnav (LLNNAV)
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDINFO', ier )
C
C*  Initialize grid library common area grdcmn.cmn
C 
	    CALL GD_INIT  ( ier )
C
C*  Initialize the DG library.
C
	    CALL DG_INTL ( ier )
	    done = .false.
	ELSE
	    done = .true.
	END IF
C
C*  Main loop to read in TAE parameters and list data.
C
	DO WHILE  ( .not. done )
C
C*  Set flag to indicate procesing will be done.
C
	    proces = .true.
C
C*  Read in the variables from user. 
C
	    CALL GDIINP  ( gdfileo, lstall, output, gdattim, glevel, 
     +			   gvcord, gfunc, iperr )
C
C*  Exit if there is an error
C
	    IF  ( iperr .ne. 0 )  THEN
	        done = .true.
	      ELSE
C
C*  Get the output units.
C
	        CALL IN_OUTT  ( output, 'GDINFO', luns, nlun, devs, 
     +				iret )
		CALL ST_FIND  ( 'T', devs, 4, ipos, ier )
		IF  ( ipos .ne. 0 )  THEN
		    pause = .true.
		  ELSE
		    pause = .false.
		END IF
C
C*  Parse user input for levels and vertical coordinate.
C
		CALL GDU_GLEV ( glevel, LLMXLV, gvcord, nlev, rlevel,
     +		                levtyp, vparm, icord, ier)
     		proces = ( ier .eq. 0 )
C
C*  Parse user input for a list of parameters.
C
		CALL ST_LCUC ( gfunc, gfunc, ier )
		CALL ST_CLST ( gfunc, ';', ' ', MMPARM , prmlst,
     +		               nparm, ier )
     		proces = ( ier .eq. 0 )
C
C*		Call DG library to get the list of times.
C
		IF ( proces ) THEN
		    CALL DG_NFIL ( gdfileo, ' ', ier )
     		    proces = ( ier .eq. 0 )
	        END IF
		IF ( proces ) THEN
		    CALL ST_LCUC ( gdattim, gdattim, ier )
		    IF ( gdattim .eq. 'ALL' ) THEN
		        CALL DG_NDTM ( 'FIRST-LAST', ier )
		      ELSE
		        CALL DG_NDTM ( gdattim, ier )
		    END IF
		    CALL DG_QTMS ( LLMXGT, .true., timfnd, ntime,
     +		                   trange, ier )
		END IF
C
C*		Check GDFILE input.
C
		CALL FL_INQR ( gdfileo, exist, filnam, ier )
		IF ( proces .and. .not. exist ) THEN
C
C*	            GDFILE input is an alias, get the template.
C
		    CALL ST_CLST ( gdfileo, '|', ' ', 2, farr,
     +		                   nfarr, ier )
		    gdfile = farr (1)
		    CALL ST_NULL ( gdfile, gdfile, ngd, ier )
		    path = ' '
		    tmplt = ' '
		    CALL CTB_DTGET ( gdfile, path, tmplt, ic, is, if, 
     +			ir, ii, ion, ihb, mnb, iha, mna, mstrct,
     +			idtmch, ier )
     		    proces = ( ier .eq. 0 )
		    CALL ST_RNUL ( path, path, lens, ier )
		    CALL ST_RNUL ( tmplt, tmplt, lens, ier )
		    CALL FL_INQR ( path, exist, filnam, ier )
		    IF  ( path .eq. 'AWIPSDB' )  THEN
			proces = .true.
			awpflg = .true.
		    ELSE
			proces = exist
			path   = filnam
			awpflg = .false.
		    END IF
		    CALL ST_LSTR ( path, lp, ier )
C
C*		    Loop through times.
C
		    nt = 1
		    DO WHILE ( proces .and. nt .le. ntime )
C
C*		        Construct the grid file name.
C
			idxclm = INDEX ( timfnd (nt), ':' )
			IF ( idxclm .gt. 0 ) THEN
			    tmpdtm = timfnd (nt)(:idxclm-1)
			  ELSE
			    tmpdtm = timfnd (nt)
			END IF
			CALL FL_MNAM ( tmpdtm, tmplt, filnam, ier )
			filnam = path(:lp) // '/' // filnam
            IF ( awpflg ) THEN
                CALL ST_LSTR( filnam, lf, ier)
                filnam = filnam(:lf) // '/' // tmpdtm
			END IF
C
C*			Open the grid file.
C
			CALL GD_OPEN ( filnam, .false., LLNANL,
     +			    LLNNAV, iflno, anl, rnav, maxgrd, ier )
			proces = ( ier .eq. 0 )
			CALL GD_NGRD ( iflno, numgrd, tmfst,
     +			    tmlst, ier )
C
C*	            	List information.
C
			IF  ( nt .eq. 1 )  THEN
			    CALL GDIGEN ( filnam, anl, rnav,
     +				          numgrd, maxgrd, nlun,
     +					  luns, ier )
			END IF
C
C*		    	List grids if requested.
C
			IF  ( lstall )  THEN
			    title = .true.
			    IF ( nt .gt. 1 )  title = .false.
			    IF ( ( gdattim .eq. 'ALL' ) .and.
     +				 ( .not. awpflg ) ) THEN
                                CALL GR_LIST ( LLMXLV, nlun, luns,
     +				    iflno, pause, ' ', title, levtyp,
     +                              nlev, rlevel, icord, nparm, prmlst,
     +                              1, gdattim, ans, ier )
			      ELSE
                                CALL GR_LIST ( LLMXLV, nlun, luns,
     +				    iflno, pause, ' ', title, levtyp,
     +                              nlev, rlevel, icord, nparm, prmlst,
     +                              1, timfnd(nt), ans, ier )
			    END IF
			    CALL ST_LCUC ( ans, ans, ier )
			    proces = ans(1:1) .ne. 'E'
			END IF
			nt = nt + 1
		    END DO
C
		  ELSE IF ( proces .and. exist ) THEN
C
C*		    Open the grid file.
C
		    CALL GD_OPEN ( filnam, .false., LLNANL, LLNNAV,
     +		        iflno, anl, rnav, maxgrd, ier )
		    CALL GD_NGRD ( iflno, numgrd, tmfst, tmlst, ier )
C
C*	            List information.
C
		    CALL GDIGEN ( filnam, anl, rnav, numgrd, maxgrd,
     +		        nlun, luns, ier )
C
C*                  List grids if requested.
C
		    IF ( lstall ) THEN
			title = .true.
		        IF ( gdattim .eq. 'ALL' ) THEN
                            CALL GR_LIST ( LLMXLV, nlun, luns, iflno,
     +			        pause, ' ', title, levtyp, nlev,
     +                          rlevel, icord, nparm, prmlst, 1,
     +                          gdattim, ans, ier )
		          ELSE
C
C*		            Loop through times.
C
		            nt = 1
		            DO WHILE ( proces .and. nt .le. ntime )
			        IF ( nt .gt. 1 )  title = .false.
                                CALL GR_LIST ( LLMXLV, nlun, luns,
     +				    iflno, pause, ' ', title, levtyp,
     +                              nlev, rlevel, icord, nparm, prmlst,
     +                              1, timfnd(nt), ans, ier )
		                CALL ST_LCUC ( ans, ans, ier )
		                proces = ans(1:1) .ne. 'E'
			        nt = nt + 1
			    END DO
		        END IF
		    END IF
		END IF
	    END IF
C
C*	    Prompt for new input.
C
	    CALL IP_DYNM  ( done, ier )
C
	END DO
C
C*	Print general error messages if necessary.
C
	IF (iperr .ne. 0) CALL ER_WMSG ( 'GDINFO', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
