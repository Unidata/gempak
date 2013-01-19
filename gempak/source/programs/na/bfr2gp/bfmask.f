	 SUBROUTINE BFMASK(iflag, stid, keepers, slat, slon, icount, ier)
C************************************************************************
C* BFMASK								*
C*									*
C* This subroutine checks to see if station is in keep list.   		*
C*									*
C*									*
C* BFMASK  ( stid, keepers, icount, ok, IRET )				*
C*									*
C* Input parameters:							*
C*	stid    	CHARACTER	Station ID    			*
C*      slat                    
C*      slon
C*      selv
C*      keepers         CHARACTER       Station list to keep		*
C*      icount          INTEGER         Number of stations in list	*
C*									*
C* Output parameters:							*
C*	OK		LOGICAL		Flag (true if station is keep)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* L. Sager    		 11/04   					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*8     keepers(2000,6)
        CHARACTER*8     astat,stid,rlat,rlon
        INTEGER*8       ilatk,ilonk
  
        EQUIVALENCE     (astat, rstat)
        EQUIVALENCE     (ilatk, rlat) 
        EQUIVALENCE     (ilonk, rlon)
C*
C-----------------------------------------------------------------------
	ier  = 0
        iflag = 0       

        do k = 1, icount
           astat = keepers(k,1)
           if (stid(1:6) .eq. astat(1:6)) then
              slon1 = slon
              if(slon .gt. 180.) slon1 = 360. - slon
              ilon = int(slon1)
              ilat = int(slat)
              rlat = keepers(k,2)
              rlon = keepers(k,3)
              ilatk = ilatk/100
              ilonk = ilonk/100
              if(ilonk .gt. 180) ilonk = 360 - ilonk 
              if ((ilatk .eq. ilat) .and. (ilon .eq. ilonk)) then
                  if(keepers(k,4).eq.'00000000') then
                     keepers(k,4) = '11111111' 
                     iflag = 1
                     return
                  else
                     if(keepers(k,4) .eq. '11111111') then
                        keepers(k,4) = '22222222'
                        iflag = 1 
                        return
                     endif
                  endif
              endif
           endif 
        enddo 
C*
	RETURN
	END
