	SUBROUTINE IM_GIHD  ( imgfil, ignhdr, nhead, icompress, izoff,
     +				iret )
C************************************************************************
C* IM_GIHD								*
C*									*
C* This subroutine reads the header information from a GINI file.	*
C*									*
C* IM_GIHD  ( IMGFIL, IGNHDR, NHEAD, IRET )				*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*	IGNHDR		INTEGER (NHEAD)	Header block words		*
C*	NHEAD		INTEGER		Number of header bytes		*
C*									*
C* Output parameters:							*
C*	ICOMPRESS	INTEGER		Compression type value		*
C*	IZOFF		INTEGER		Start of compressed data	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C*					 -3 = Invalid image file format *
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C* C. Lin/EAI	 	 6/95  	change input filename -> lunmf		*
C* J. Cowie/COMET	10/95	Add image size maximum check, set xy	*
C*				image scaling to 1.0			*
C* J. Cowie/COMET	 4/96	Added calib setting			*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Chiswell/UNIDATA	12/97	Handle byte flipping			*
C* J. Cowie/COMET	12/97	Added imradf, removed unused variables,	*
C*				add cross section products		*
C* S. Jacobs/NCEP	12/97	Added TB_NIDS; Removed NIPRM.PRM	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* S. Jacobs/NCEP	 5/99	Added setting of imbswp for byte-swap	*
C* S. Jacobs/NCEP	 2/00	Use the 1st time in the file, not 2nd	*
C* S. Jacobs/NCEP	11/00	Changed to call IM_NIDH			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C*
	INTEGER		nhead, igstrt
	INTEGER*4	ignhdr (nhead)
C*
	LOGICAL		negflg
	CHARACTER	defdir*(LLMXLN)
C------------------------------------------------------------------------
	iret = 0
	icompress = 0
	izoff = 0
C
C*	Open the file and read the Message Header and Product
C*	Description blocks
C
	CALL FL_DOPN  ( imgfil, nhead, .false., lunmf, iret )
        READ (UNIT=lunmf, REC=1, IOSTAT=io) ignhdr
C
        IF  ( io .ne. 0 )  THEN
            iret = -2
            RETURN
        END IF
	CALL FL_CLOS  ( lunmf, ier )
C
C*	See if this is looks like an GINI header
C
	istart = 21 - 1
        nbytes = 1
        negflg = .false.
        CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ihdr1, ier )
        istart = 25 - 1
        CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ihdr2, ier )
        IF ( ihdr1 .eq. 10 ) THEN
            igstrt = 21
          ELSE IF ( ihdr2 .eq. 10 ) THEN
            igstrt = 25
          ELSE
            iret = -4
        END IF
C
        nbytes = 1
        negflg = .false.
        CALL MV_BTOI ( ignhdr, igstrt, nbytes, negflg, isnesdis, ier )
C
C*	If the first byte is "1", then if this is a GINI, it is uncompressed
	IF ( isnesdis .eq. 1 ) THEN
	    RETURN
	END IF
C
C*	See if this image is zlib compressed starting after the outside WMO header
C*	Uncompress the first block of image
C
	nblocks = 1
	iobytes = 4 * nhead
	defdir(1:1) = CHNULL
	CALL ST_NULL ( imgfil, imgfil, lens, ier )
	CALL CLZ_RFIL ( imgfil, defdir, nblocks, igstrt, iobytes, ignhdr, 
     +			ibin, ibout, ier)

	IF ( ier .eq. 0 ) THEN
	    icompress = 1
	    izoff = igstrt + ibin
	END IF

	RETURN
	END
