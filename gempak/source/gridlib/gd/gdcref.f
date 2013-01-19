	SUBROUTINE GD_CREF  ( filnam, navsz,  rnvblk, ianlsz, anlblk, 
     +			      ihdrsz, maxgrd, iacss, iret )
C************************************************************************
C* GD_CREF								*
C*									*
C* This subroutine creates a new GEMPAK5 grid file.  If MAXGRD is zero	*
C* or negative, it will default to 400.  IHDRSZ is the length of	*
C* the grid header which will be stored with every grid.  This		*
C* header is intended to save offsets from a base grid, but is not	*
C* currently used.  IHDRSZ should usually be set to 2.			*
C*									*
C* GD_CREF  ( FILNAM, NAVSZ, RNVBLK, IANLSZ, ANLBLK, IHDRSZ,		*
C*            MAXGRD, IACSS, IRET )					*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	NAVSZ		INTEGER		Navigation blck length (LLNNAV)	*
C*	RNVBLK (NAVSZ)	REAL		Navigation block		*
C*	IANLSZ		INTEGER		Analysis block length (LLNANL)	*
C*	ANLBLK (IANLSZ)	REAL		Analysis block			*
C*	IHDRSZ		INTEGER		Grid header length		*
C*	MAXGRD		INTEGER		Max number of grids in file	*
C*									*
C* Output parameters:							*
C*	IACSS		INTEGER		Grid file access number		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = file cannot be created	*
C*					-13 = grid header too long	*
C*					-17 = File number limit reached.*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	11/88	Save anl and nav blocks in common	*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL			*
C* K. Brill/HPC		12/03	Return the file access number		*
C* S. Jacobs/NCEP	 6/04	Fixed use of LLGDHD to define hdr size	*
C* R. Tian/SAIC		 1/05	Modified to handle file create/close	*
C* R. Tian/SAIC		 3/05	Set value for mgrid			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*) 	filnam
	REAL		rnvblk (*), anlblk (*)
C*
	CHARACTER	kcolnm(10)*4, krownm*4, fhdnam(2)*4, prtnam*4,
     +			parms*4
	INTEGER		ifhlen (2), ifhtyp (2)
	PARAMETER	( MMGRID = 400 )
C
	DATA		krownm / 'GRID' /, fhdnam / 'NAVB', 'ANLB' /
	DATA		ifhlen / LLNNAV, LLNANL /,
     +			ifhtyp / 2 * MDREAL /
	DATA		kcolnm / 'GDT1', 'GTM1', 'GDT2', 'GTM2', 
     +				 'GLV1', 'GLV2', 'GVCD', 'GPM1',
     +				 'GPM2', 'GPM3' /
	DATA		prtnam / 'GRID' /, parms / 'GRID' /
C------------------------------------------------------------------------
	iret = 0
	nucode = .true.
C
C*	Check that maximum number of grids to save is within bounds.
C
	IF  ( maxgrd .le. 0 )  THEN
	    ngrid = MMGRID
	  ELSE IF  ( maxgrd .ge. MMHDRS )  THEN
	    ngrid = MMHDRS - 1
	  ELSE
	    ngrid = maxgrd
	END IF
C
C*	Set variables necessary for DM package.
C
	iftype = MFGD
	ifsrce = MFGRID
	nfhdrs = 2
	nrow   = 1
	nrkeys = 1
	ncol   = ngrid
	nckeys = 10
	nprt   = 1
	nparm  = 1
	maxprm = nparm
	ityprt = MDGRID
C
C*	Check length of data header.  Add 2 words to store kx and ky.
C
	IF  ( ihdrsz .gt. LLGDHD )  THEN
	    iret = -13
	    RETURN
	  ELSE IF  ( ihdrsz .gt. LLGDHD-2 )  THEN
	    idthdr = ihdrsz
	  ELSE
	    idthdr = ihdrsz + 2
	END IF
C
C*	Create the file.
C
	CALL DM_CRET (filnam, iftype, ifsrce, nfhdrs, fhdnam, ifhlen,
     +                ifhtyp, nrow,   nrkeys, krownm, ncol,   nckeys, 
     +	              kcolnm, nprt,   prtnam, idthdr, ityprt, nparm, 
     +	              maxprm, parms,  iscale, iofset, ibits,  igdfln,
     +                iret)
C
C*      MMFILE files have been opened, close the least recently
C*      used file and create again.
C
        IF ( iret .eq. -3 ) THEN
            ismlst = MXFLNM
            DO i = 1, MMFILE
                IF ( iflacc(i) .ne. 0 .and. iflacc(i) .lt. ismlst )
     +          THEN
                    ismlst = iflacc (i)
                END IF
            END DO
            CALL GD_CLOS ( ismlst, ier )
	    CALL DM_CRET (filnam, iftype, ifsrce, nfhdrs, fhdnam,
     +	                  ifhlen, ifhtyp, nrow,   nrkeys, krownm,
     +                    ncol,   nckeys, kcolnm, nprt,   prtnam,
     +                    idthdr, ityprt, nparm,  maxprm, parms,  
     +                    iscale, iofset, ibits,  igdfln, iret)
        END IF
C
C*	Write error messages if file was not created.
C
	IF ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'DM', iret, filnam, ier )
	    iret = -1
	    CALL ER_WMSG ( 'GD', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Write row header so it is not empty.
C
	irowhd = 1
	irow   = 1
	CALL DM_WRWH  ( igdfln, irow, irowhd, ipos, ier )
C
C*	Write navigation block to file.
C
	CALL DM_WFHR  ( igdfln, fhdnam (1), rnvblk, navsz, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'DM', iret, ' ', ier )
	    iret = -1
	    CALL ER_WMSG ( 'GD', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Write analysis block to file if length is non-zero.
C
	IF  ( ianlsz .gt. 0 )  THEN
	  CALL DM_WFHR  ( igdfln, fhdnam (2), anlblk, ianlsz, iret )
	  IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'DM', iret, ' ', ier )
	    iret = -1
	    CALL ER_WMSG ( 'GD', iret, ' ', ier )
	    RETURN
	  END IF
	END IF
C
C*	Load common area.
C
	mgrid  ( igdfln ) = ngrid
	kgrid  ( igdfln ) = 0
	ktgrid ( igdfln ) = 0
	ksrtl  ( 1,1,igdfln ) = 0
	gdwrt  ( igdfln ) = .true.
	igrdfn ( igdfln ) = igdfln
	khdrln ( igdfln ) = idthdr
	DO  ij = 1, navsz
	    savnav ( ij, igdfln ) = rnvblk ( ij )
	END DO
	lnavbl ( igdfln ) = navsz
	DO  ij = 1, ianlsz
	    savanl ( ij, igdfln ) = anlblk ( ij )
	END DO
	lanlbl ( igdfln ) = ianlsz
	nflnum = nflnum + 1
	IF ( nflnum .gt. MXFLNM ) THEN
	    iret = -17
	    RETURN
	END IF
	iflacc (igdfln) = nflnum
	gdflnm (igdfln) = filnam
	iacss = nflnum
C*
	RETURN
	END
