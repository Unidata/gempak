	SUBROUTINE GD_OPNF  ( filnam, wrtflg, igdfln, navsz, rnvblk,
     +			      ianlsz, anlblk, ihdrsz, maxgrd, iret )
C************************************************************************
C* GD_OPNF								*
C*									*
C* This subroutine opens an existing GEMPAK grid file.  If the		*
C* file requires shared, write access, the subroutine GD_OPNR		*
C* should be used.							*
C*									*
C* GD_OPNF ( FILNAM, WRTFLG, IGDFLN, NAVSZ, RNVBLK, IANLSZ, ANLBLK, 	*
C*           IHDRSZ, MAXGRD, IRET )					*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*	WRTFLG		LOGICAL		Flag for write access		*
C*									*
C* Output parameters:							*
C*	IGDFLN		INTEGER		File number			*
C*	NAVSZ		INTEGER		Navigation block length		*
C*	RNVBLK (NAVSZ)	REAL		Navigation block		*
C*	IANLSZ		INTEGER		Analysis block length		*
C*	ANLBLK (IANLSZ)	REAL		Analysis block			*
C*	IHDRSZ		INTEGER		Grid header length		*
C*	MAXGRD		INTEGER		Maximum number of grids		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = file cannot be opened	*
C*					 -7 = not a GEMPAK5 grid file	*
C*					 -8 = nav cannot be read	*
C*					-13 = grid header too long	*
C*					-14 = file name is blank	*
C*					-99 = cannot use with new code	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 4/90	Added error for blank file name		*
C* K. Brill/HPC		12/03	Check the nucode flag (must be false)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*) 	filnam
	LOGICAL		wrtflg
	REAL		rnvblk (*), anlblk (*)
C*
	LOGICAL		shrflg
C------------------------------------------------------------------------
C*	Check for blank name.
C
	IF ( nucode ) THEN
	    iret = -99
	    RETURN
	END IF
	CALL ST_LSTR  ( filnam, lenf, ier )
	IF  ( lenf .eq. 0 )  THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Open the file.
C
	shrflg = .false.
	CALL GD_OFIL  ( filnam, wrtflg, shrflg, igdfln, navsz, rnvblk,
     +			ianlsz, anlblk, ihdrsz, maxgrd, iret )
C*
	RETURN
	END
