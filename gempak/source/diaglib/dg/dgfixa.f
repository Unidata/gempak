	SUBROUTINE DG_FIXA  ( area, proj, areout, prjout, iret )
C************************************************************************
C* DG_FIXA								*
C*									*
C* This subroutine takes AREA and replaces GRID or DSET with the grid   *
C* area, EXTEND with the extend area, and DATA with the data area.	*
C* GRID or DSET is obtained from the navigation block; EXTEND and	*
C* DATA are obtained from the analysis block.				*
C*									*
C* DG_FIXA  ( AREA, PROJ, AREOUT, PRJOUT, IRET )			*
C*									*
C* Input parameters:							*
C*	AREA		CHAR*		Area				*
C*	PROJ		CHAR*		Projection			*
C*									*
C* Output parameters:							*
C*	AREOUT		CHAR*		New area			*
C*	PRJOUT		CHAR*		New projection			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-54 = grid file open failed	*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06	Fortran wrapper of DGC_FIXA		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	area, areout, proj, prjout
C*
	CHARACTER	tmpare*(LLMXLN), tmppro*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( area, tmpare, nt, ier )
	CALL ST_NULL ( proj, tmppro, nt, ier )
	CALL DGC_FIXA ( tmpare, tmppro, areout, prjout, iret )
	CALL ST_RNUL ( areout, areout, nt, ier )
	CALL ST_RNUL ( prjout, prjout, nt, ier )
C*
	RETURN
	END
