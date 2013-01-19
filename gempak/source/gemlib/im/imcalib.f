	SUBROUTINE IM_CALIB( iarray, ilen, ioff, iret)
C************************************************************************
C* IM_CALIB                                                             *
C*                                                                      *
C* This subroutine reads the calibration block of an AREA file, and	*
C* stores the colorbar levels in the appropriate common block variables.*
C* If the calibration block is a PROD, then the pixel and data values	*
C* are retrieved for the colorbar levels.				*
C*                                                                      *
C* IM_CALIB  ( IARRAY, ILEN, IOFF, IRET )				*
C*                                                                      *
C* Input parameters:                                                    *
C*      IARRAY          INTEGER*        Header array			*
C*	ILEN		INTEGER		Header array length		*
C*	IOFF		INTEGER		Byte offset for calibration	*
C*									*
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* Chiz/Unidata		07/00		Created for CIMSS products	*
C*					Initially, only PROD is handled.*
C* S. Chiswell/Unidata	 2/04		Added imcmn.cmn for imcalbar	*
C************************************************************************

	INTEGER		iarray(*), ilen, ioff, iret

	CHARACTER	tval*4

	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'imcmn.cmn'

	iret = 0

	istart = (ioff / 4) + 1
C
C*	See if this is a known Calibration type
C
	CALL ST_ITOC ( iarray (istart), 1, tval, ier )

C	IF ( ier .eq. 0 ) THEN
        if((tval .eq. 'PROD').or.(tval .eq. 'N0R').or.
     +		(tval .eq. 'FIRE' ) ) then

c          cmcalb = tval
           cmcalb = 'PROD'
C
C*	   Read in min and max pixel and corresponding data
C*	   values. Scale value is for data points.
C
	   iminval = iarray( istart + 1 )
	   imaxval = iarray( istart + 2 )
	   iminpix = iarray( istart + 3 )
	   imaxpix = iarray( istart + 4 )

C
C*	   Un-Swap calibration text values if necessary
C
	   iloc = istart + 5
	   if (imbswp .eq. 1) 
     +		ier = MV_SWP4 ( 1, iarray (iloc), iarray (iloc) )
	   CALL ST_ITOC ( iarray (iloc), 1, tval, ier )

           if(ier .eq. 0) then
              cmbunt = tval
           else
              cmbunt = 'Unk'
           endif

	   iscaleval = iarray( istart + 6 )
	   if(iscaleval .eq. 0) iscaleval = 1

C
C*	   Determine colorbar levels and store commonblock values.
C 
	   ratio = float(imaxval - iminval)/float(imaxpix - iminpix)

	   immnpx = iminpix
	   immxpx = imaxpix
           iminpix = iminpix + 1
           imaxpix = imaxpix + 1
           do i=1,256
              if(i.eq.iminpix) then
                 CALL ST_INCH ( iminval/iscaleval, cmblev (i), ier )
              else if(i.eq.imaxpix) then
                 CALL ST_INCH ( imaxval/iscaleval, cmblev (i), ier )
              else if((i.gt.iminpix).and.(i.lt.imaxpix)) then
                    IF ( mod ( i-1,16) .eq. 0) THEN
                        level = nint( (i-iminpix) * ratio) + iminval
                        CALL ST_INCH ( level/iscaleval, 
     +                                 cmblev (i), ier )
                    ELSE
                        cmblev (i) = ' '
                    END IF
              else
                 cmblev(i) = ' '
              endif
           end do
        endif

	imcalbar = 1

	RETURN
	END
