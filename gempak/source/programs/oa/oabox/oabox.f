	PROGRAM OABOX
C************************************************************************
C* OABOX								*
C*									*
C* This program draws a box around the grid, extend or data area.	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL			*
C* L. Williams/EAI	 7/94	Removed call to OABUPD			*
C* M. Linda/GSC		12/95	Now calling GG_MAPS instead of GG_SMAP	*
C* S. Jacobs/NCEP	 6/96	Changed null string to blanks		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* T. Lee/GSC		11/00	Changed call sequence of GR_FIXA	*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	line*(LLMXLN), device*(LLMXLN), region*(LLMXLN),
     +			garea*(LLMXLN),	proj*(LLMXLN), gdfile*(LLMXLN)
C
	LOGICAL		done, respnd, proces, scflag
	REAL		rltln (4), x (5), y (5)
	CHARACTER	aaa*24, imgfil*1, prjout*72
	REAL		rnav (LLNNAV), anl (LLNANL)
C-----------------------------------------------------------------------
C* 	Initialize TAE and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'OABOX', ier )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( iperr )
            mode = 1
	    CALL GG_INIT  ( mode, iperr )
	    IF  ( iperr .eq. 0 )  THEN 
	        done = .false.
	      ELSE
	        done = .true.
	    END IF
	END IF
C
C*	Loop through program drawing box.
C
	DO WHILE  (.not. done)
	    proces = .true.
	    CALL OABINP  ( line, device, region, garea, proj, gdfile, 
     +			   iperr )
	    IF  ( iperr .ne. 0 )  THEN
	        done = .true.
	      ELSE
C
C*	        Set device.
C
	        CALL GG_SDEV  ( device, iret )
	        IF  ( iret .ne. 0 )  proces = .false.
C
C*		Set map projection.
C
	        IF  ( proces )  THEN
		    imgfil = ' '
		    CALL GG_MAPS ( proj, garea, imgfil, idrpfl, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Open grid file.
C
		IF  ( proces )  THEN
		    CALL GD_OPNF  ( gdfile, .false., igdfln, navsz,
     +				    rnav, ianlsz, anl, ihdrsz,
     +				    maxg, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		    END IF
		END IF
C
C*		Process LINE variable.
C
		IF  ( proces )  THEN
		    CALL IN_LINE  ( line, 0., 1, icolor, itype, iwidth,
     +				    ilabel, smth, fltr, scflag, ier )
		    IF  ( icolor .eq. 0 )  THEN
			CALL ER_WMSG  ( 'OABOX', -4, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*		Get bounds of analysis region.
C
		IF  ( proces )  THEN
		    CALL GR_FIXA  ( igdfln, region, proj, 
     +				    aaa, prjout, iret )
		    IF  ( iret .eq. 0 )  THEN
			CALL ST_RLST  ( aaa, ';', RMISSD, 4, rltln, 
     +					n, ier )
			IF  ( n .ne. 4 )  iret = -5
		    END IF
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'OABOX', -5, region, ier )
			proces = .false.
		    END IF
		END IF
C
C*		Set corners.
C
		IF  ( proces )  THEN
		    x (1) = rltln (1)
		    y (1) = rltln (2)
		    x (2) = rltln (3)
		    y (2) = rltln (2)
		    x (3) = rltln (3)
		    y (3) = rltln (4)
		    x (4) = rltln (1)
		    y (4) = rltln (4)
		    x (5) = x (1)
		    y (5) = y (1)
C
C*		    Get current line attributes and set line and color.
C
		    CALL GQLINE  ( iltypo, ilthwo, iwido, iwhwo, ier )
		    CALL GSCOLR  ( icolor, ier )
		    CALL GSLINE  ( itype, 0, iwidth, 0, ier )
C
C*		    Draw box around anlare.
C
		    CALL GLINE  ( 'M', 5, x, y, iret )
		    CALL GEPLOT  ( ier )
C
C*		    Reset line and update global information.
C
		    CALL GSLINE  ( iltypo, 0, iwido, 0, ier )
C
C*		    Close grid file.
C
		    CALL GD_CLOS  ( igdfln, ier )
	        END IF
	    END IF
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Final errors.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'OABOX', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
