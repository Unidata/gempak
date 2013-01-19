<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes"/>


  <!--
      xmlBufr_common.xsl
      
      Purpose: Supply common routines for xmlBufr_sierra.xsl,
      xmlBufr_tango.xsl, and xmlBufr_zulu.xsl.  These are
      used to produce XML documents used for creation of
      the GFA bufr message.  
      
      Routines:
      
      productStatus - Generate appropriate bufr codes for the Status
        of NRML, COR, and AMD.
      
      timePeriod - Generate 2 time periods via function dateTime
      
      dateTime - Parse date string into date and time attributes.
        date in YYYY-MM-DD format, time in HH:MM format.

      latlons - Recursive template to parse lat string and lon
        string into lat/lon attributes to element named vertex
        
      procFlightLevel - Parse Flight Level Line accordingly
        based on a numeric flight level, or flight level
        string of 'SFC' or 'FZL'

      horizDesc - Horizontal Description - Ref. Desc. 3-01-028 
        (Horizontal section of a feature described as polygon, circle,
        line or point)
            
      descOfFeature - Description of Feature - Ref. Desc. 3-01-027
       (Description of a feature in 3-D or 2-D)
       
      GFAIdObsOrFcstLoc - GFA ID and Observed Or Forecast Location -
        Ref. Desc 3-16-054
        
      getObscCode - Get Obscuration Code - Generate appropriate bufr codes
       for the obscurations of  FG, BR, HZ, FU, CLDS, PCPN, BLSN.
      
      parseObsc - Parse Obscuration - Recursive Template to parse DUE-TO
        line for Obscurations
            
      Change Log:
      
      L. Hinson/AWC 01/06 Initial Coding
      L. Hinson/AWC 03/06 Updated for 5.9.2
      L. Hinson/AWC 06/08 Updated to distinguish between tops/bases on freezing
                          Levels or regular Flight Levels, and set bufrCode
                          attribute to a value of 34, 35, 36, or 37.
      L. Hinson/AWC 04/09 Re-Issued
  -->

  <xsl:template name="productStatus">
    <xsl:param name="Status" select="."/>
    <xsl:choose>
       <xsl:when test="Status='NRML'"> 
	     <xsl:attribute name="type">Normal</xsl:attribute>
         <xsl:attribute name="bufrCode">0</xsl:attribute>
       </xsl:when>
       <xsl:when test="Status='COR'"> 
	     <xsl:attribute name="type">Correction</xsl:attribute>
         <xsl:attribute name="bufrCode">1</xsl:attribute>
       </xsl:when>
       <xsl:when test="Status='AMD'"> 
	     <xsl:attribute name="type">Amendment</xsl:attribute>
         <xsl:attribute name="bufrCode">2</xsl:attribute>
       </xsl:when>
       <xsl:when test="Status='NEW'">
         <xsl:attribute name="type">Amendment</xsl:attribute>
         <xsl:attribute name="bufrCode">2</xsl:attribute>
       </xsl:when>
       <xsl:when test="Status='CAN'">
         <xsl:attribute name="type">Cancel</xsl:attribute>
         <xsl:attribute name="bufrCode">4</xsl:attribute>
       </xsl:when>
       <xsl:otherwise>
	     <xsl:attribute name="type">Normal</xsl:attribute>
         <xsl:attribute name="bufrCode">0</xsl:attribute>
       </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
       
  <xsl:template name="timePeriod">
    <xsl:param name="issueTime" select="."/>
    <xsl:param name="untilTime" select="."/>
      <xsl:element name="validFrom">
        <xsl:call-template name="datetime">
          <xsl:with-param name="dtstr" select="$issueTime"/>
        </xsl:call-template>
      </xsl:element>
      <xsl:element name="validUntil">
        <xsl:call-template name="datetime">
	  <xsl:with-param name="dtstr" select="$untilTime"/>
	</xsl:call-template>
      </xsl:element>
  </xsl:template>

  <xsl:template name="datetime">
    <xsl:param name="dtstr" select="."/>
    <xsl:attribute name="date"> <xsl:value-of select="substring($dtstr,1,4)"/>-<xsl:value-of select="substring($dtstr,5,2)"/>-<xsl:value-of select="substring($dtstr,7,2)"/></xsl:attribute>
    <xsl:attribute name="time"> <xsl:value-of select="substring($dtstr,9,2)"/>:<xsl:value-of select="substring($dtstr,11,2)"/>:00</xsl:attribute>
  </xsl:template>

  <xsl:template name="latlons">
    <xsl:param name="latstr" select="."/>
    <xsl:param name="lonstr" select="."/>
    <xsl:param name="splitString" select="' '"/>
    <xsl:choose>
      <xsl:when test="contains($latstr,$splitString)">
        <xsl:element name="vertex">
	  <xsl:attribute name="latitude"> <xsl:value-of select="format-number(substring-before($latstr,$splitString), '#00.00' )"/> </xsl:attribute>
	  <xsl:attribute name="longitude"> <xsl:value-of select="format-number(substring-before($lonstr,$splitString), '#000.00' )"/> </xsl:attribute>
        </xsl:element>
        <xsl:call-template name="latlons">
	  <xsl:with-param name="latstr" select="substring-after($latstr,$splitString)"/>
	  <xsl:with-param name="lonstr" select="substring-after($lonstr,$splitString)"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="vertex">
          <xsl:attribute name="latitude"> <xsl:value-of select="format-number($latstr, '#00.00' )"/> </xsl:attribute>
	  <xsl:attribute name="longitude"> <xsl:value-of select="format-number($lonstr, '#000.00' )"/> </xsl:attribute>  
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    
  <xsl:template name="procFlightLevel">
    <xsl:param name="fl" select="."/>
    <xsl:choose>
      <xsl:when test="contains($fl,'FZL')">
         <xsl:if test="contains($fl,'BASE')">
           <flightLevelSig type="Freezing Level Base" bufrCode="34"/>
           <xsl:if test="string-length(substring-after($fl,'BASE:')) &gt; 2">
             <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'BASE:')"/> </flightLevel>
           </xsl:if>
         </xsl:if>
         <xsl:if test="contains($fl,'TOP')">
           <flightLevelSig type="Freezing Level Top" bufrCode="35"/>
           <xsl:if test="string-length(substring-after($fl,'TOP:')) &gt; 2">
             <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'TOP:')"/> </flightLevel>
           </xsl:if>
         </xsl:if>
      </xsl:when>
      <xsl:when test="contains($fl,'FLIGHT LEVEL BASE')">
        <flightLevelSig type="Flight Level Base" bufrCode="36"/>
        <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'BASE:')"/> </flightLevel>
      </xsl:when>
      <xsl:when test="contains($fl,'FLIGHT LEVEL TOP')">
        <flightLevelSig type="Flight Level Top" bufrCode="37"/>
        <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'TOP:')"/> </flightLevel>
      </xsl:when>          
      <xsl:when test="contains($fl,'SFC')">
         <flightLevelSig type="Surface" bufrCode="20"/>
      </xsl:when>
      <xsl:when test="contains($fl,'BTN FL')">
        <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'BTN FL')"/> </flightLevel>
      </xsl:when>        
      <xsl:when test="contains($fl,'BTN')">
        <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'BTN ')"/> </flightLevel>
      </xsl:when>
      <xsl:when test="contains($fl,'FL')">
        <flightLevel units="hundreds feet"> <xsl:value-of select="substring-after($fl,'FL')"/> </flightLevel>
      </xsl:when>
      <xsl:otherwise>
        <flightLevel units="hundreds feet"> <xsl:value-of select="$fl"/> </flightLevel>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
     
  <xsl:template name="horizDesc">
    <xsl:param name="nPts" select="."/>
    <xsl:param name="latPts" select="."/>
    <xsl:param name="lonPts" select="."/>
    <xsl:param name="dueTo" select="."/>
    <xsl:param name="typeLimit" select="."/>
    <xsl:param name="fltLvl" select="."/>
    <horizontalDescription>
      <xsl:if test="string-length($fltLvl)>=2">
        <xsl:choose>
          <xsl:when test="$typeLimit=0">
            <flightLevelLimit type="Above" bufrCode="0"/>
          </xsl:when>
          <xsl:when test="$typeLimit=1">
            <flightLevelLimit type="Above and Including" bufrCode="1"/>
          </xsl:when>
          <xsl:when test="$typeLimit=2">
            <flightLevelLimit type="Below" bufrCode="2"/>
          </xsl:when>
          <xsl:when test="$typeLimit=3">
            <flightLevelLimit type="Below and Including" bufrCode="3"/>
          </xsl:when>
          <!-- <xsl:otherwise>
            <flightLevelLimit type="Missing" bufrCode="7"/>
          </xsl:otherwise> -->
        </xsl:choose>
        <xsl:call-template name="procFlightLevel">
          <xsl:with-param name="fl" select="$fltLvl"/>
        </xsl:call-template>
      </xsl:if>
      <numberVertexPoints> <xsl:value-of select="$nPts"/> </numberVertexPoints>
      <xsl:call-template name="latlons">
        <xsl:with-param name="latstr" select="normalize-space($latPts)"/>
        <xsl:with-param name="lonstr" select="normalize-space($lonPts)"/>
      </xsl:call-template>
    </horizontalDescription>
  </xsl:template>
 
  <!-- descOfFeature - (Description of Feature)
  Note: The number of Horizontal Sections (levels) encoded is dependent on the 
  type of phenomena being encoded.  Ref. International SIGMET Sequence Proposal.
  Flight Level Significance will be set if phenomena is at SFC or a Freezing
  Level, otherwise omitted in XML and set to missing in BUFR.  Type Limit
  will be set or omitted as noted.  If omitted, will be coded as code figure 7,
  "missing" in BUFR.
  1) IFR - 2 levels with type Limits of 1 & 3 - Ref. footnote 1c
     on CIG BLW 010 phenemena.
  2) Mountain Obscuration - 1 level.
  3) Turbulence - 1 to 2 levels.
     1 level for phenomena extending from flight level to sfc, type
     limit of 3.  For 2 levels, type limit omitted.
  4) Strong Surface Wind - 1 level with Flight Level Significance
     set to SFC, or code 20. Type limit omitted.
  5) Low Level Wind Shear - 1 level with type limit of 3, "BLW and
     including"
  6) Icing - 1 to 2 levels.
     1 level for phenomena extending from flight level to sfc, type
     of limit of 3.  For 2 levels, type of limit will be omitted.  If lower
     level is FZL, then Flight level Significance of 34 will be used.
  7) Freezing Levels - 1 level for flight levels SFC, 040, 080, 120.
       Flight Level Significance set as follows:
       Surface (code 20) for flight level of SFC.
       Freezing Level Base (code 34)
       Freezing Level Top (code 35)
       Flight Level Base (code 36)
       Flight Level Top (code 37)
  -->
  <xsl:template name="descOfFeature">
    <xsl:param name="closeFlg" select="."/>
    <xsl:param name="nPts" select="."/>
    <xsl:param name="latPts" select="."/>
    <xsl:param name="lonPts" select="."/>
    <xsl:param name="dueTo" select="."/>
    <xsl:param name="top" select="."/>
    <xsl:param name="base" select="."/>
    <xsl:param name="fzltop" select="."/>
    <xsl:param name="fzlbase" select="."/>
    <objectDescription>
      <xsl:element name="dimensionSig">
        <xsl:choose>
          <xsl:when test="$closeFlg='1'">
            <xsl:attribute name="type">Area</xsl:attribute>
            <xsl:attribute name="bufrCode">2</xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="type">Line</xsl:attribute>
            <xsl:attribute name="bufrCode">1</xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="contains($dueTo,'CIG BLW 010')">
            <numberOfLevels>2</numberOfLevels>
            <xsl:call-template name="horizDesc">
              <xsl:with-param name="nPts" select="$nPts"/>
              <xsl:with-param name="latPts" select="$latPts"/>
              <xsl:with-param name="lonPts" select="$lonPts"/>
              <xsl:with-param name="dueTo" select="$dueTo"/>
              <xsl:with-param name="typeLimit" select="1"/>
              <xsl:with-param name="fltLvl" select="string('010')"/>
            </xsl:call-template>
            <xsl:call-template name="horizDesc">
              <xsl:with-param name="nPts" select="$nPts"/>
              <xsl:with-param name="latPts" select="$latPts"/>
              <xsl:with-param name="lonPts" select="$lonPts"/>
              <xsl:with-param name="dueTo" select="$dueTo"/>
              <xsl:with-param name="typeLimit" select="3"/>
              <xsl:with-param name="fltLvl" select="string('010')"/> 
            </xsl:call-template>
          </xsl:when>
	  <xsl:when test="$top != $base and $base != 'SFC' and string-length($fzlbase) = 0">
            <numberOfLevels>2</numberOfLevels>
            <xsl:call-template name="horizDesc">
              <xsl:with-param name="nPts" select="$nPts"/>
              <xsl:with-param name="latPts" select="$latPts"/>
              <xsl:with-param name="lonPts" select="$lonPts"/>
              <xsl:with-param name="dueTo" select="$dueTo"/>
              <xsl:with-param name="typeLimit" select="7"/>
              <xsl:with-param name="fltLvl" select="concat('FLIGHT LEVEL BASE:', $base)"/>
            </xsl:call-template>
            <xsl:call-template name="horizDesc">
              <xsl:with-param name="nPts" select="$nPts"/>
              <xsl:with-param name="latPts" select="$latPts"/>
              <xsl:with-param name="lonPts" select="$lonPts"/>
              <xsl:with-param name="dueTo" select="$dueTo"/>
              <xsl:with-param name="typeLimit" select="7"/>
              <xsl:with-param name="fltLvl" select="concat('FLIGHT LEVEL TOP:', $top)"/>
            </xsl:call-template>
          </xsl:when>
	  <xsl:when test="$top != $base and $base = 'SFC'">
            <numberOfLevels>1</numberOfLevels>
            <xsl:call-template name="horizDesc">
              <xsl:with-param name="nPts" select="$nPts"/>
              <xsl:with-param name="latPts" select="$latPts"/>
              <xsl:with-param name="lonPts" select="$lonPts"/>
              <xsl:with-param name="dueTo" select="$dueTo"/>
              <xsl:with-param name="typeLimit" select="3"/>
              <xsl:with-param name="fltLvl" select="concat('FLIGHT LEVEL TOP:', $top)"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="$base = 'FZL'">
            <xsl:if test="string-length($fzlbase) > 1">
              <numberOfLevels>3</numberOfLevels>
              <xsl:call-template name="horizDesc">
                <xsl:with-param name="nPts" select="$nPts"/>
                <xsl:with-param name="latPts" select="$latPts"/>
                <xsl:with-param name="lonPts" select="$lonPts"/>
                <xsl:with-param name="dueTo" select="$dueTo"/>
                <xsl:with-param name="typeLimit" select="7"/>
                <xsl:with-param name="fltLvl" select="concat($base,' BASE:',$fzlbase)"/>
              </xsl:call-template>
              <xsl:call-template name="horizDesc">
                <xsl:with-param name="nPts" select="$nPts"/>
                <xsl:with-param name="latPts" select="$latPts"/>
                <xsl:with-param name="lonPts" select="$lonPts"/>
                <xsl:with-param name="dueTo" select="$dueTo"/>
                <xsl:with-param name="typeLimit" select="7"/>
                <xsl:with-param name="fltLvl" select="concat($base,' TOP:',$fzltop)"/>
              </xsl:call-template>
              <xsl:call-template name="horizDesc">
                <xsl:with-param name="nPts" select="$nPts"/>
                <xsl:with-param name="latPts" select="$latPts"/>
                <xsl:with-param name="lonPts" select="$lonPts"/>
                <xsl:with-param name="dueTo" select="$dueTo"/>
                <xsl:with-param name="typeLimit" select="7"/>
                <xsl:with-param name="fltLvl" select="concat('FLIGHT LEVEL TOP:', $top)"/>
              </xsl:call-template>
            </xsl:if>
          </xsl:when>  
	  <xsl:otherwise>
	    <numberOfLevels>1</numberOfLevels>
            <xsl:call-template name="horizDesc">
              <xsl:with-param name="nPts" select="$nPts"/>
              <xsl:with-param name="latPts" select="$latPts"/>
              <xsl:with-param name="lonPts" select="$lonPts"/>
              <xsl:with-param name="dueTo" select="$dueTo"/>
              <xsl:with-param name="typeLimit" select="7"/>
              <xsl:with-param name="fltLvl" select="$top"/>
            </xsl:call-template>
	  </xsl:otherwise>
        </xsl:choose>
      </xsl:element>
    </objectDescription>
  </xsl:template>
    
  <xsl:template name="GFAIdObsOrFcstLoc">
    <xsl:param name="id" select="."/>
    <xsl:param name="fcstHr" select="."/>
    <xsl:param name="validFrom" select="."/>
    <xsl:param name="validUntil" select="."/>
    <xsl:param name="closeFlg" select="."/>
    <xsl:param name="nPts" select="."/>
    <xsl:param name="latPts" select="."/>
    <xsl:param name="lonPts" select="."/>
    <xsl:param name="dueTo" select="."/>
    <xsl:param name="top" select="."/>
    <xsl:param name="base" select="."/>
    <xsl:param name="fzltop" select="."/>
    <xsl:param name="fzlbase" select="."/>
    <gfaIdObsOrFcstLoc>
      <id> <xsl:value-of select="$id"/> </id>
      <xsl:element name="timeSig">
        <xsl:choose>
          <xsl:when test="contains($fcstHr,'-') or $fcstHr!='0' or $fcstHr!='0-0'">
            <xsl:attribute name="type">Forecast</xsl:attribute>
            <xsl:attribute name="bufrCode">4</xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="type">Analysis</xsl:attribute>
            <xsl:attribute name="bufrCode">16</xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <validTime>
          <xsl:call-template name="timePeriod">
            <xsl:with-param name="issueTime" select="$validFrom"/>
            <xsl:with-param name="untilTime" select="$validUntil"/>
          </xsl:call-template>
        </validTime>
        <xsl:call-template name="descOfFeature">
          <xsl:with-param name="closeFlg" select="$closeFlg"/>
          <xsl:with-param name="nPts" select="$nPts"/>
          <xsl:with-param name="latPts" select="$latPts"/>
          <xsl:with-param name="lonPts" select="$lonPts"/>
          <xsl:with-param name="dueTo" select="$dueTo"/>
          <xsl:with-param name="top" select="$top"/>
	  <xsl:with-param name="base" select="$base"/>
          <xsl:with-param name="fzltop" select="$fzltop"/>
          <xsl:with-param name="fzlbase" select="$fzlbase"/>
        </xsl:call-template>
      </xsl:element>
    </gfaIdObsOrFcstLoc>
  </xsl:template>
      
  <xsl:template name="getObscCode">
    <xsl:param name="type" select="."/>
    <xsl:attribute name="type"> <xsl:value-of select="$type"/> </xsl:attribute>
    <xsl:choose>
      <xsl:when test="$type='FG'">
        <xsl:attribute name="bufrFlag">1</xsl:attribute>
      </xsl:when>
      <xsl:when test="$type='BR'">
        <xsl:attribute name="bufrFlag">7</xsl:attribute>
      </xsl:when>
      <xsl:when test="$type='HZ'">
        <xsl:attribute name="bufrFlag">8</xsl:attribute>
      </xsl:when>
      <xsl:when test="$type='FU'">
        <xsl:attribute name="bufrFlag">9</xsl:attribute>
      </xsl:when>
      <xsl:when test="$type='CLDS'">
        <xsl:attribute name="bufrFlag">14</xsl:attribute>
      </xsl:when>
      <xsl:when test="$type='PCPN'">
        <xsl:attribute name="bufrFlag">15</xsl:attribute>
      </xsl:when>
      <xsl:when test="$type='BLSN'">
        <xsl:attribute name="bufrFlag">13</xsl:attribute>
        <xsl:attribute name="bufrCode">6</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="bufrFlag">1</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
        
  <xsl:template name="parseObsc">
    <xsl:param name="obsc" select="."/>
    <xsl:param name="x" select="."/>
    <xsl:choose>
      <xsl:when test="contains($obsc,'/')">
        <xsl:element name="obscuration">
          <xsl:call-template name="getObscCode">
            <xsl:with-param name="type" select="substring-before($obsc,'/')"/>
          </xsl:call-template>
        </xsl:element>
        <xsl:call-template name="parseObsc">
          <xsl:with-param name="obsc" select="substring-after($obsc,'/')"/>
	  <xsl:with-param name="x" select="$x + 1"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="obscuration">
          <xsl:call-template name="getObscCode">
            <xsl:with-param name="type" select="$obsc"/>
          </xsl:call-template>
        </xsl:element>
        <numberObscurations><xsl:value-of select="$x"/></numberObscurations>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="hdr">
    <hdr>
      <xsl:element name="validFrom">
        <xsl:call-template name="datetime">
          <xsl:with-param name="dtstr" select="issueTime"/>
        </xsl:call-template>
      </xsl:element>
      <xsl:element name="validUntil">
        <xsl:call-template name="datetime">
	  <xsl:with-param name="dtstr" select="untilTime"/>
	</xsl:call-template>
      </xsl:element>
    </hdr>
  </xsl:template>
</xsl:stylesheet>
