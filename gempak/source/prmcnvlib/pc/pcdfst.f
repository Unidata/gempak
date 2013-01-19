	SUBROUTINE PC_DFST  ( noutpm, outprm, chrflg, cmpflg, ncomp, 
     +			      iret )
C************************************************************************
C* PC_DFST								*
C*									*
C* This subroutine defines the station parameters to be returned when	*
C* PC_CMST is called.  PC_INIT must be called to define the dataset	*
C* parameters before this subroutine is called.  This subroutine	*
C* should only be used in programs where the station parameters		*
C* will be accessed separately from the level parameters.  CMPFLG	*
C* indicates whether the parameters are computable.			*
C*									*
C* The current station parameters are:  STID, STNM, SELV, SLAT, SLON,	*
C* SPRI, STIM, FELV and various stability indices.		        *
C*									*
C* PC_DFST  ( NOUTPM, OUTPRM, CHRFLG, CMPFLG, NCOMP, IRET )		*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*	OUTPRM (NOUTPM)	CHAR*4		Output parameter names		*
C*									*
C* Output parameters:							*
C*	CHRFLG (NOUTPM)	LOGICAL		Character type flag		*
C*	CMPFLG (NOUTPM)	LOGICAL		Computable flag			*
C*	NCOMP		INTEGER		Number of computable parms	*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal completion	*
C*				 	  -4 = PC_INIT not called	*
C*				 	  -5 = invalid NOUTPM		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	9/84						*
C* M. desJardins/GSFC	10/86	Reset internal flags set in previously	*
C* M. desJardins/GSFC	 9/87	Fixed call to PC_SINT for LIFT		*
C* M. desJardins/GSFC	 9/88	Redefined meaning of flags		*
C* M. desJardins/GSFC	 7/89	Initialize chrflg			*
C* G. Huffman/USRA	 8/89	BRCH, CAPE, CINS, EQLV, LFCV		*
C* M. desJardins/GSFC	11/89	Added conditions and STIM		*
C* M. desJardins/GSFC	 7/90	Move initialization of scmflg from DFLV	*
C* M. desJardins/GSFC	 7/90	Change initialization of station parms	*
C* J. Whistler/SSAI	 2/91	Added PWAT to station parameters	*
C* T. Lee/GSC		 8/97	Added BRCV, CAPV, CINV, EQTV, LFCT,	*
C*				and LFTV				*
C* D. Kidwell/NCEP	 7/98   Added FELV                              *
C* A. Hardy/GSC		 3/99	Added priority parameter SPRI		*
C* T. Lee/SAIC		 6/03	Added LHAN, MHAN, HHAN			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'pccmn.cmn'
C*
	LOGICAL  	chrflg(*), cmpflg(*)
	CHARACTER*(*)	outprm (*)
C*
	LOGICAL		pfound
	CHARACTER*4	p
C-------------------------------------------------------------------------
C*	Check for errors in input values.
C
	IF  ( .not. dsflg )  THEN
	    iret = -4
	  ELSE IF ( ( noutpm .lt. 1 ) .or. ( noutpm .gt. MAXPRM ) ) THEN
	    iret = -5
	  ELSE
	    iret = 0
	ENDIF
	IF  ( iret .lt. 0 )  RETURN
C
C*	Set output flag values and number of computable parameters.
C
	DO  i = 1, noutpm
	    cmpflg (i) = .false.
	    chrflg (i) = .false.
	    scmflg (i) = .false.
	END DO
C
	ncomp  = 0
	nfound = 0
	ksprm  = noutpm
C
C*	Set up tables 5,6,7 to be used to compute the basic parameters 
C*	for the stability indices.
C
	IF  ( .not. sindxf )  THEN
	    index = 5
	    CALL PC_METH  ( index, jdsprm, dsparm, MSTNPM, splist,
     +			    scomp, n, ier )
C
C*	    If interpolation flags are on, set up the interpolation 
C*	    tables.
C
	    index1 = 6
	    index2 = 7
	    IF  ( jntflg  .and.  inton  .and.  (jcord .ne. 0) )  THEN
		CALL PC_SINT  ( MSTNPM, splist, scomp, index1, index2, 
     +				ier )
	    END IF
	    sindxf = .true.
	END IF
C
C*	Check to see if parameters are recognized station parameters.
C
	DO  i = 1, noutpm
	    p = outprm (i)
	    pfound = .false.
	    IF  ( ( p .eq. 'STID' ) .or. ( p .eq. 'STNM' ) )  THEN
		chrflg (i) = .true.
		pfound = .true.
C------------------------------------------------------------------------
	      ELSE IF  ( ( p .eq. 'SELV' ) .or. ( p .eq. 'SLAT' ) .or. 
     +			 ( p .eq. 'SLON' ) .or. ( p .eq. 'STIM' ) .or.
     +			 ( p .eq. 'SPRI' ) .or. ( p .eq. 'FELV' ) ) THEN
		pfound = .true.
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'SHOW' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		ENDIF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'KINX' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'TOTL' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'CTOT' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'VTOT' )  THEN
		IF  ( scomp (1) .and. scomp (2) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LIFT' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LFTV' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'SWET' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) .and. 
     +		      scomp (4) .and. scomp (5) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'BRCH' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) .and. 
     +		      scomp (4) .and. scomp (5) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'BRCV' )  THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) .and. 
     +		      scomp (4) .and. scomp (5) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'CAPE' )  THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'CAPV' )  THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'CINS' )  THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'CINV' )  THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'EQLV' )  THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'EQTV' )  THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LFCT' )   THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LFCV' )   THEN
		IF  ( scomp (2) .and. scomp (3) .and. scomp (6) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'MLTH' )  THEN
		IF  ( scomp (1) .and. scomp (2) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'MLMR' )   THEN
		IF  ( scomp (1) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LCLP' )   THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LCLT' )   THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'PWAT' )   THEN
		IF  ( scomp (1) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'LHAN' )   THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'MHAN' )   THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	      ELSE IF  ( p .eq. 'HHAN' )   THEN
		IF  ( scomp (1) .and. scomp (2) .and. scomp (3) )  THEN
		    pfound = .true.
		END IF
C------------------------------------------------------------------------
	    END IF
	    IF  ( pfound )  THEN
		cmpflg (i) = .true.
		stnprm (i) = p
		scmflg (i) = .true.
		nfound = nfound + 1
C
C*		Added the ELSE statement to reset flags from previous
C*		calls to PC_DFST.  10/86
C
	      ELSE
		scmflg (i) = .false.
		stnprm (i) = ' '
	    END IF
	END DO
	ncomp = nfound
C
C*	Turn condition flag off.
C
	stncnd = .false.
C*
	RETURN
	END
