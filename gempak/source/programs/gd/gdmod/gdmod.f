	PROGRAM  GDMOD
C************************************************************************
C* PROGRAM GDMOD							*
C*									*
C* This program moves grids from an input grid file to an output 	*
C* grid file.								*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88						*
C* M. desJardins/GSFC	 3/89	Added grid packing			*
C* L. Williams/EAI	 7/94	Removed call to GDOUPD			*
C* K. Tyle/GSC           8/96   Added FL_MFIL to search for file type   *
C* S. Maxwell/GSC	 9/96	Added capability to select grids based  *
C*  				on specific user input			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* M. Li/SAIC           04/04   Added new parameter grdhdr              *
C* R. Tian/SAIC         10/04   Changes for time/file mngmnt            *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gdoutf*(LLMXLN), gpack*(LLMXLN),
     +			gdattim*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), gfunc*(LLMXLN), grdhdr*(LLMXLN)
C*
	INTEGER  	iarhdr (2)	
	LOGICAL		respnd, done, proces
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDMOD', ier )
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
C*  Main processing loop.
C
	DO WHILE  ( .not. done )
C
C*  Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*  Read in the variables from the TAE.
C
	    CALL GDOINP  ( gdfile, gdoutf, gpack, gdattim, 
     +	  		   glevel, gvcord, gfunc, grdhdr, iperr )
C
C*  Exit if there is an error
C
	    IF ( iperr .ne. 0 ) THEN
	        done = .true.
	      ELSE
C
C*	        List grids if requested and add.
C
                CALL ST_ILST ( grdhdr, '/', IMISSD, 2, iarhdr, nn,
     +		               ier )
                ihzrmp = iarhdr (1)
                idrct  = iarhdr (2)
C
C*	        Add grids to output file.
C
	        CALL GDOADD  ( gdfile, gdoutf, gpack, gdattim, glevel,
     +		               gvcord, gfunc, ihzrmp, idrct, ier )
C
C*	        Prompt for next set of grids to be deleted.
C
	        CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF (iperr .ne. 0) CALL ER_WMSG ( 'GDMOD', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
