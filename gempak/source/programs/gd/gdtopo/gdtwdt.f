	SUBROUTINE GDTWDT ( igdfln, grid, ikx, iky, time, nbits, iret )
C************************************************************************
C* GDTWDT								*
C*									*
C* This subroutine will write the topography to a GEMPAK grid file.	*
C*									*
C* GDTWDT ( IGDFLN, GRID, IKX, IKY, TIME, IRET )			*
C*									*
C* Input parameters:							*
C*	IGDFLN		INTEGER			Grid file number	*
C*	GRID(*)		REAL			Grid values		*
C*	IKX		INTEGER			Number of X points	*
C*	IKY		INTEGER			Number of Y points	*
C*	TIME(2)		CHAR*(*)		Grid times		*
C*	NBITS		INTEGER			Number of packing bits	*
C* Output parameters:							*
C*	IRET		INTEGER			Return code		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	11/91						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	time(2)
	REAL		grid(*)
C*
	CHARACTER	parm*4
	INTEGER		ighdr(LLGDHD), level(2), nbits
	LOGICAL		rewrit
C------------------------------------------------------------------------
	ipktyp   = MDGGRB
c	nbits    = 16
	ivcord   = 1
	level(1) = 0
	level(2) = -1
	rewrit   = .true.
	parm     = 'TOPO'
C
	DO i=1,LLGDHD
	   ighdr(i) = 0
	END DO
	CALL GD_WPGD ( igdfln, grid, ikx, iky, ighdr, time, level,
     +		       ivcord, parm, rewrit, ipktyp, nbits, iret )
C*
	RETURN
	END
