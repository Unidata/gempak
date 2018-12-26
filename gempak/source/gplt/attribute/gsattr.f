	SUBROUTINE GSATTR ( iret )
C************************************************************************
C* GSATTR								*
C*									*
C* This subroutine resets the device plotting attributes and the map or	*
C* graph projection.  The routine is called if a new window is created,	*
C* an old window is resized, or the user switches from one window to	*
C* another.  The contents of DEVCHR must have already been queried	*
C* before calling this routine.						*
C*									*
C* GSATTR ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/96						*
C* M. Linda/GSC		 8/96	Added DSICNG, DSSPCL, DSTURB		*
C* S. Jacobs/NCEP	10/96	Added DSFRNT				*
C* S. Jacobs/NCEP	10/96	Added check for XWP driver		*
C* S. Jacobs/NCEP	 2/97	Removed width from call to DSFRNT	*
C* D. Keiser/GSC	 3/97	Added DSSPLN				*
C* E. Safford/GSC	 6/97	Added DSTXSY				*
C* C. Lin/EAI            6/97   Consider 'S' coordinate                 *
C*                              UPDDXY -> UPDSXY                        *
C* S. Maxwell/GSC	 6/97	Added DSFILL				*
C* S. Jacobs/NCEP	 9/97	Removed DSTXSY; Changed call to DSTEXT	*
C* S. Jacobs/NCEP	 2/98	Added DSSMTH				*
C* I. Durham/GSC	 3/98	Added DSDARR				*
C* I. Durham/GSC	 3/98	Added DSHASH				*
C* S. Jacobs/NCEP	 6/98	Changed call to GSFRNT to use REAL size	*
C* A. Hardy/GSC         10/98	Added DSCMBO                            *
C* S. Jacobs/NCEP	 5/99	Added DSRDUC				*
C* S. Jacobs/NCEP	 6/00	Added check for SAT proj, call GSATMG	*
C* S. Guan/NCEP          5/17   Modified for NETCDF4 Himawari data      *
C*                              Add goe4, call GSATMG4                  *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVWIN.CMN'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
        CHARACTER goe4*4
	iret = NORMAL
C
C*      Move the screen bounds into /XYDEF/.
C
        IF ( ileft .le. iright ) THEN
            ibndls = ileft + isxoff
            ibndrs = ileft + isxoff + iswdth
          ELSE
            ibndls = iright + isxoff + iswdth
            ibndrs = iright + isxoff
        END IF
C
        IF ( itop .le. ibot ) THEN
            ibndts = itop + isyoff
            ibndbs = itop + isyoff + ishght
          ELSE
            ibndts = ibot + isyoff + ishght
            ibndbs = ibot + isyoff
        END IF
C
C*	Move the device bounds into /XYDEF/.
C
	ixbndl = ileft
	iybndb = ibot
	ixbndr = iright
	iybndt = itop
C
C*	Calculate all the coordinate information.
C
	idx = ABS ( iright - ileft )
	idy = ABS ( itop   - ibot  )
C
C*	Check for valid bounds.
C
	IF ( ( idx .eq. 0 ) .or. ( idy .eq. 0 ) ) THEN
	    iret = NDCHAR
	  ELSE
	    CALL UPDSXY
	END IF
C
C*	Send current attribute information to device.
C
	CALL DSCOLR ( kcolr, lcolr, ier )
C
	CALL DSDASH ( rszdsh, sszdsh, ier )
C
	CALL DSLINE ( kltyp, klthw, klwid, klwhw, lltyp, llthw,
     +		      llwid, llwhw, ier )
C
	CALL DSSPLN ( ksltyp, kslstr, ksldir, rslsiz, kslwid,
     +		      lsltyp, lslstr, lsldir, sslsiz, lslwid, ier )
C
	CALL DSFRNT ( kfcod, rpipsz, kpipst, kpipdr,
     +		      lfcod, spipsz, lpipst, lpipdr, ier )
C
	CALL DSMRKR ( kmark, kmkhw, rmksz, kmkwid, lmark, lmkhw,
     +		      smksz, lmkwid, ier )
C
	CALL DSTEXT ( ktxfn, ktxhw, rtxsz, ktxwid,
     +		      kbrdr, krrotn, kjust,
     +		      ltxfn, ltxhw, stxsz, ltxwid,
     +		      lbrdr, lrrotn, ljust, ier )
C
	CALL DSARRW ( rwasz, rwahsz, karwid, kartyp, swasz,
     +		      swahsz, larwid, lartyp, ier )
C
	CALL DSDARR ( rwdasz, rdahsz, kdarwd, kdartp, swdasz,
     +		      sdahsz, ldarwd, ldartp, ier )
C
	CALL DSBARB ( rwbsz, kbrwid, kbrtyp, swbsz, lbrwid,
     +		      lbrtyp, ier )
C
	CALL DSHASH ( rhshsz, khwid, klwidh, shshsz,
     +		      lhwid, llwidh, ier )
C
	CALL DSSKY  ( rskysz, ksktyp, kskwid, sskysz, lsktyp,
     +		      lskwid, ier )
C
	CALL DSWTHR ( rwtrsz, kwtwid, swtrsz, lwtwid, ier )
C
	CALL DSPTND ( rptnsz, kptwid, sptnsz, lptwid, ier )
C
	CALL DSPWTH ( rpwtsz, kpwwid, spwtsz, lpwwid, ier )
C
	CALL DSCTYP ( rctsz,  kctwid, sctsz,  lctwid, ier )
C
	CALL DSICNG ( rcersz, kcewid, scersz, lcewid, ier )
C
	CALL DSSPCL ( rsprsz, kspwid, ssprsz, lspwid, ier )
C
	CALL DSTURB ( rtursz, ktuwid, stursz, ltuwid, ier )
C
	CALL DSFILL ( rfilsz, kfltyp, sfilsz, lfltyp, ier )
C
	CALL DSSMTH ( ksmtyp, rdens, ketype, rtensn,
     +		      lsmtyp, sdens, letype, stensn, ier )
C
	CALL DSCMBO ( rcsysz, kcsywd, scsysz, lcsywd, ier )
C
	CALL DSRDUC ( rrfilt, srfilt, ier )
C
C*	Set the map/graph attributes.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) ) THEN
	    IF  ( nmode (ncurwn) .eq. 1 ) THEN
		IF  ( igmode .ne. 1 ) CALL GSMODE ( 1, ier )
		xlmmgn = umarg ( ncurwn, 1 )
		ybmmgn = umarg ( ncurwn, 2 )
		xrmmgn = umarg ( ncurwn, 3 )
		ytmmgn = umarg ( ncurwn, 4 )
		cszm   = uszm  ( ncurwn )
C
                CALL ST_ITOC ( nnav(1,ncurwn), 1, goe4, ier )
		IF  ( wcproj(ncurwn) .ne. 'SAT' )  THEN
		    CALL GSMPRJ ( wcproj(ncurwn), uangle(ncurwn,1),
     +				  uangle(ncurwn,2), uangle(ncurwn,3),
     +				  ulatll(ncurwn), ulonll(ncurwn),
     +				  ulatur(ncurwn), ulonur(ncurwn), ier )
		ELSE IF ( goe4 .eq. 'GOE4' ) THEN
C       For NETCDF4 Himawari data
		    CALL GSATMG4 ( wsatfl(ncurwn), narea(1,ncurwn),
     +				  nnav(1,ncurwn), nixlef(ncurwn),
     +				  niytop(ncurwn), nixrit(ncurwn),
     +				  niybot(ncurwn), ier )
                ELSE
                    CALL GSATMG ( wsatfl(ncurwn), narea(1,ncurwn),
     +                            nnav(1,ncurwn), nixlef(ncurwn),
     +                            niytop(ncurwn), nixrit(ncurwn),
     +                            niybot(ncurwn), ier ) 
		END IF
C
	    ELSE IF  ( nmode (ncurwn) .eq. 2 ) THEN
		IF  ( igmode .ne. 2 ) CALL GSMODE ( 2, ier )
		xlgmgn = umarg ( ncurwn, 1 )
		ybgmgn = umarg ( ncurwn, 2 )
		xrgmgn = umarg ( ncurwn, 3 )
		ytgmgn = umarg ( ncurwn, 4 )
		cszg   = uszg  ( ncurwn )
		CALL GSGRAF ( nxtyp(ncurwn), nytyp(ncurwn),
     +			      uyxrat(ncurwn), uxl(ncurwn),
     +			      uyb(ncurwn), uxr(ncurwn),
     +			      uyt(ncurwn), ier )
	    END IF
	END IF
C*
	RETURN
	END
