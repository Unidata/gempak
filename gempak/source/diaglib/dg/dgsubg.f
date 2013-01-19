	SUBROUTINE DG_SUBG  ( ijskip, imll, jmll, imur, jmur, iret )
C************************************************************************
C* DG_SUBG								*
C*									*
C* This subroutine sets the internal subset grid given the reference	*
C* grid navigation set in GPLT and the map projection set in GPLT.	*
C* If the reference grid is globe wrapping with the addition of an	*
C* extra grid column, then the navigation set in GPLT must be that for	*
C* the grid with the extra column.					*
C* 									*
C* The subset grid is larger by five grid points than that strictly	*
C* needed to cover the map projection area.  This extension permits	*
C* more accurate computation of derivatives.  The subset grid relative	*
C* coordinates of the region strictly needed for the map are returned.	*
C* 									*
C*									*
C* IJSKIP is parsed by IN_GSKP.  IJSKIP information is entered using	*
C* the following format, where items in square brackets are optional:	*
C*									*
C*	IJSKIP = Iskip[;Istart][;Iend][/Jskip[;Jstart][;Jend]],		*
C*									*
C*	IJSKIP=Y[ES], or IJSKIP=N[O]					*
C*									*
C* The following rules apply in using IJSKIP input:			*
C*									*
C* 1.  If only Iskip is entered, then I and J skips are Iskip.  The	*
C*     beginning points and ending points are determined by querying	*
C*     the display projection to find the area on the reference grid	*
C*     needed to cover it.						*
C*									*
C* 2.  If any bounding value is omitted, it is determined automatically *
C*     by querying the display projection as in 1 above.		*
C*									*
C* 3.  If IJSKIP is blank or NO, skipping is not used to determine the	*
C*     internal grid navigation.					*
C* 									*
C* 4.  If IJSKIP is YES, all skip parameters are determined		*
C*     automatically.							*
C* 									*
C* DG_SUBG  ( IJSKIP, IMLL, JMLL, IMUR, JMUR, IRET )			*
C*									*
C* Input parameters:							*
C*	IJSKIP		CHAR*		User input for skip subsetting	*
C*									*
C* Output parameters:							*
C*	IMLL		INTEGER		Lower left map I bound		*
C*	JMLL		INTEGER		Lower left map J bound		*
C*	IMUR		INTEGER		Upper right map I bound		*
C*	JMUR		INTEGER		Upper right map J bound		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-37 = no ref grid navigation set*
C*					-38 = glb wrap grd inconsistency*
C*					-39 = map projection is not set *
C*					-40 = subset grd bound error	*
C*					-41 = subset grid is too big	*
C*					-43 = cannot rearrange grid	*
C*					-44 = error set subset grid nav	*
C*					-48 = both I bounds required	*
C**									*
C* Log:									*
C* R. Tian/SAIC		 3/06	Fortran wrapper of DGC_SUBG		*
C* S. Gilbert/NCEP	 7/07	added maxgrid argument to dgc_subg	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	ijskip
C*
	CHARACTER	tmpijs*(LLMXLN)
C------------------------------------------------------------------------
        maxgrid = LLMXGD
	CALL ST_NULL ( ijskip, tmpijs, nt, ier )
	CALL DGC_SUBG ( tmpijs, maxgrid, imll, jmll, imur, jmur, iret )
C*
	RETURN
	END
