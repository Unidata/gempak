	SUBROUTINE OAGGRD  ( gdfile, kx, ky, grltln, eltln, dltln, 
     +			     deltan, deltax, deltay, maxgrd, iret )
C************************************************************************
C* OAGGRD								*
C*									*
C* This subroutine creates a new grid file.				*
C*									*
C* OAGGRD  ( GDFILE, KX, KY, GRLTLN, ELTLN, DLTLN, DELTAN, DELTAX,	*
C*           DELTAY, MAXGRD, IRET )					*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	KX		INTEGER		Number of points in x-dir	*
C*	KY		INTEGER		Number of points in y-dir	*
C*	GRLTLN (4)	REAL		Grid area			*
C*	ELTLN (4)	REAL		Extend area			*
C*	DLTLN (4)	REAL		Data area			*
C*	DELTAN		REAL		Station spacing			*
C*	DELTAX		REAL		X spacing			*
C*	DELTAY		REAL		Y spacing			*
C*	MAXGRD		INTEGER		Maximum number of grids		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/87	Added GD calls for GEMPAK4		*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*) 	gdfile
	REAL 		grltln (4), eltln (4), dltln (4)
C*
	PARAMETER 	( IHDRSZ = 2 )
	REAL 		anlblk (LLNANL), rnvblk (LLNNAV)
C------------------------------------------------------------------------
C*	Create a navigation block for evenly spaced lat/lon grid.
C
	CALL GR_MNAV  ( 'CED', kx, ky, grltln (1), grltln (2),
     +			grltln (3), grltln (4), 0., 0., 0., .false.,
     +			rnvblk, ier )
C
C*	Create an analysis block. 
C
	CALL GR_MBAN  ( deltan, deltax, deltay, grltln, eltln, dltln, 
     +			anlblk, ier )
C
C*	Create the grid file.
C
	CALL GD_CREF  ( gdfile, LLNNAV, rnvblk, LLNANL, anlblk, IHDRSZ, 
     +			maxgrd, iflno, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -1
	END IF
	CALL GD_CLOS  ( iflno, ier )
C*
	RETURN
	END
