	SUBROUTINE DM_RPKG  ( iflno, isword, nword, rdata, mword, iret )
C************************************************************************
C* DM_RPKG								*
C*									*
C* This subroutine reads a packed grid from a file.			*
C*									*
C* DM_RPKG  ( IFLNO, ISWORD, NWORD, RDATA, MWORD, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Length of data record		*
C*									*
C* Output parameters:							*
C*	RDATA (MWORD)	REAL		Grid data			*
C*	MWORD		INTEGER		Number of grid points		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-31 = invalid packing		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C* T. Piper/GSC		 7/01	Added 'if (irw .eq. 3)' check		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	REAL		rdata (*)
C*
	INTEGER		iarray (4)
	REAL		rarray (3)
	LOGICAL		miss
C------------------------------------------------------------------------
	iret = 0
C
C*	First read packing type from file.
C
	CALL DM_RINT  ( iflno, isword, 1, ipktyp, iret )
	IF  ( iret .ne. 0 )  RETURN
	iiword = isword + 1
	lendat = nword - 1
C
C*	Read real data if there is no packing.
C
	IF  ( ipktyp .eq. MDGNON )  THEN
	    CALL DM_RFLT  ( iflno, iiword, lendat, rdata, iret )
	    mword = lendat
	    RETURN
	END IF
C
C*	Read rest of header depending on type.
C
	IF  ( ipktyp .eq. MDGDIF )  THEN
	    iiw = 4
	    irw = 3
	  ELSE
	    iiw = 3
	    irw = 2
	END IF
	CALL DM_RINT  ( iflno, iiword, iiw, iarray, iret )
	IF  ( iret .ne. 0 )  RETURN
	iiword = iiword + iiw
	lendat = lendat - iiw
	CALL DM_RFLT  ( iflno, iiword, irw, rarray, iret )
	IF  ( iret .ne. 0 )  RETURN
	iiword = iiword + irw
	lendat = lendat - irw
C*
	nbits  = iarray (1) 
	misflg = iarray (2)
	IF  ( misflg .eq. 0 )  THEN
	    miss = .false.
	  ELSE
	    miss = .true.
	END IF
	kxky   = iarray (3) 
	mword  = kxky
	kx     = iarray (4) 
	ref    = rarray (1) 
	scale  = rarray (2) 
	if ( irw .eq. 3 ) difmin = rarray (3) 
C
C*      Call 'C' subfunction to read and unpack the data from the file.
C
	CALL DM_RPKGC ( iflno, iiword, lendat, ipktyp, kxky, nbits, ref, 
     &                  scale, miss, difmin, kx, rdata, iret )
C*
	RETURN
	END
