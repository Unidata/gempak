	SUBROUTINE IM_NCDF4  ( imgfil, iret )
C************************************************************************
C* IM_NCDF4								*
C*									*
C* This subroutine reads the header information from an AWIPS style	*
C* NetCDF4 file, and sets the navigation.				*
C*									*
C* IM_NCDF4  ( IMGFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C**									*
C* Log:									*
C* S. Guan/NCEP          6/15   Created                                 *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
        INCLUDE         'AREAFL.CMN'  
C*
	CHARACTER*(*)	imgfil
C*
	CHARACTER	filnam*160, proj*4
        INTEGER         area(64), nav(640)
C------------------------------------------------------------------------
	iret = 0
C
C*	Add a NULL to the end of the file name.
C
	CALL ST_NULL ( imgfil, filnam, lenf, ier )
C
C*	Read the NetCDF header and get necessary information.
C
	CALL IM_RCDF4 ( filnam, area, nav, imsorc, imtype,
     +			imdate, imtime, imdpth, clat, clon, 
     +			ixll, ixur, iyll, iyur, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*      Set the remaining image definition common variables.
C
        imnpix = ixur
        imnlin = iyur
        imldat = imnpix * imnlin
        rmxysc = 1.0
        imradf = 0
        imleft = 1
        imtop  = 1
        imrght = imnpix
        imbot  = imnlin
        imbot  = iyur
        imrght = ixur
        iproj = 1
        imdpth = 1
           
        DO  i = 1, 64
            iadir(i) = area (i) 
        END DO
        DO  i = 1, 640
            ianav(i) = nav(i)
        END DO
C
C*	Set the map projection.
C
        CALL GSATMG4 (imgfil, area, nav,ixll, iyur, ixur, iyll, ier )
C*
	RETURN
	END
