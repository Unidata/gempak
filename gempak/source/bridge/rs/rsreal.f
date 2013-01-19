c  This subroutine takes the output from SMDECO and loads it into an 
c   mks real array.  Each element in PLACES corresponds to an element
c   in DATA, and the value of the element indicates the destination 
c   element in RDATA.  A zero in places means that the data element is
c   to be ignored.
c   Created 20-FEB-1989 by John Nielsen, MIT
c   Modified 4/89 by Jim Cowie (NPS) to change handling of items 7,8
c   This program has not been Gempacized, but ought to work fine. On
c   the other hand, you can probably live without using it.
c    - J. Nielsen, 2/92
c   Input variables:
      subroutine rs_real(data, places, rdata, default, missing)
cOutput from SMDECO
      integer data(200)
cDestination array
      integer places(200)
cMissing data code in DATA
      integer default
c   Output variables:
cMissing data code in RDATA
      real missing
c   Units:
c	PRECIPITATION		mm
c	OBSERVATION PERIODS	hours
c	CLOUD HEIGHTS		ft*100
c	VISIBILITY		mi
c	SPEEDS			m/s
c	TEMPERATURE		C
c	PRESSURE		mb
c	SNOW, ICE THICKNESS	cm
c	WAVE HEIGHTS		m
c	WAVE PERIODS		s
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!11
cReal mks data for GEMPAK
      real rdata(*)
cRegion, identifier, day, hour
      do i = 1, 4
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cLatitude, longitude (north and east are positive)
      do i = 5, 6
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i)) / 10.
      else
      rdata(places(i)) = missing
      end if
      end if
cSix-hourly precip
      end do
      if (places(7) .gt. 0) then
      if (data(7) .ne. default) then
      rdata(places(7)) = float(data(7)) / 10.
      if (data(8) .ne. default) then
      if (data(8) .ne. 6) rdata(places(7)) = missing
      end if
      else
      rdata(places(7)) = missing
      end if
      end if
cPrecip period, current and past weather
      do i = 8, 11
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
cCeiling
      end do
      if (places(12) .gt. 0) then
      if (data(12) .ne. default) then
      rdata(places(12)) = (float(data(12)) * (39.37 / 12.)) / 100.
      else
      rdata(places(12)) = missing
      end if
cVisibility
      end if
      if (places(13) .gt. 0) then
      if (data(13) .ne. default) then
      rdata(places(13)) = (float(data(13)) / 100.) * .61
      else
      rdata(places(13)) = missing
      end if
      end if
cSky cover, wind direction	
      do i = 14, 15
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
cWind speed
      end do
      if (places(16) .gt. 0) then
      if (data(16) .ne. default) then
      if (data(16) .lt. 500) then
      rdata(places(16)) = float(data(16))
      else
      rdata(places(16)) = float(data(16) - 500) / 1.93
      end if
      end if
      end if
cTemperature, dew point, station and sea level pressure
      do i = 17, 20
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i)) / 10.
      else
      rdata(places(i)) = missing
      end if
      end if
cPressure tendency code
      end do
      if (places(21) .gt. 0) then
      if (data(21) .ne. default) then
      rdata(places(21)) = float(data(21))
      else
      rdata(places(21)) = missing
      end if
      end if
c3 & 24 hour change, max & min temp, 24 hour precip
      do i = 22, 26
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i)) / 10.
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cSnow depth, state of ground, ship motion
      do i = 27, 30
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
cWater temperature
      end do
      if (places(31) .gt. 0) then
      if (data(31) .ne. default) then
      rdata(places(31)) = float(data(31)) / 10.
      else
      rdata(places(31)) = missing
      end if
      end if
cWave info, ice accretion
      do i = 32, 42
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      if (((i .eq. 33) .or. (i .eq. 36)) .or. (i .eq. 39)) then
      rdata(places(i)) = rdata(places(i)) / 10.
      end if
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cWind shift, temperature change
      do i = 43, 48
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
cMaximum sustained wind speed
      end do
      if (places(49) .gt. 0) then
      if (data(49) .ne. default) then
      if (data(49) .lt. 500) then
      rdata(places(49)) = float(data(49))
      else
      rdata(places(49)) = float(data(49) - 500) / 1.93
      end if
      end if
      end if
cTime of peak wind; cloud types and motions
      do i = 50, 61
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cParticular cloud cover
      do i = 62, 74, 3
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cParticular cloud type
      do i = 63, 75, 3
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cParticular cloud height
      do i = 64, 76, 3
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = (float(data(i)) * (39.37 / 12.)) / 100.
      else
      rdata(places(i)) = missing
      end if
      end if
cBelow cloud description code
      end do
      if (places(77) .gt. 0) then
      if (data(77) .ne. default) then
      rdata(places(77)) = float(data(77))
      else
      rdata(places(77)) = missing
      end if
      end if
cSnow, water, precip period, tide code
      do i = 78, 82
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = missing
      end if
      end if
      end do
cTide departure, city temp, max, min
      do i = 83, 86
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      rdata(places(i)) = float(data(i)) / 10.
      else
      rdata(places(i)) = missing
      end if
      end if
cCity precip (unknown units; assume hundredths)
      end do
      if (places(87) .gt. 0) then
      if (data(87) .ne. default) then
      rdata(places(87)) = (float(data(87)) * 25.4) / 100.
      else
      rdata(places(87)) = missing
      end if
      end if
cMax sustained wind, max/min winds?
      do i = 88, 90
      if (places(i) .gt. 0) then
      if (data(i) .ne. default) then
      if (data(i) .lt. 500) then
      rdata(places(i)) = float(data(i))
      else
      rdata(places(i)) = float(data(i) - 500) / 1.93
      end if
      end if
      end if
cTime of buoy or cman ob (hhmm)
      end do
      if (places(91) .gt. 0) then
      if (data(91) .ne. default) then
      rdata(places(91)) = float(data(91))
      else
      rdata(places(91)) = missing
      end if
      end if
cClean up
      do i = 92, 200
      if (places(i) .gt. 0) then
      rdata(places(i)) = missing
      end if
      end do
      return 
      end

