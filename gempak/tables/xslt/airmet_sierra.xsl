<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:import href="no_hazard_report.xsl"/>
    <xsl:import href="get_status.xsl"/>
    <xsl:import href="get_update.xsl"/>
    <xsl:import href="get_attention_line.xsl"/>
    <xsl:import href="output_header.xsl"/>
    <xsl:import href="output_footer.xsl"/>
    <xsl:import href="ok_to_use.xsl"/>
    <xsl:import href="output_outlook.xsl"/>
    <xsl:import href="set_hazard_name.xsl"/>
    <xsl:import href="make_conditions.xsl"/>
    
 <xsl:output method="xml"/>
    
    <!--  SigmetKey is used to uniquely identify sigmet references that may
          occur in more than one airmet.  Regardless of how many times a given
          sigmet is referenced we only want a single sigmet in the text report.
     -->
    <xsl:key name="SigmetKey" 
            match="smear[hazard='IFR']/ReferToSigmet"
              use="."/>


    <!--
        airmet_sierra.xsl
    
        This is an xslt template document.  It is to be used to process an xml 
	document that conforms to the airmet.xsd schema and produce text output 
	in the form of an Sierra Airmet report.
    
        The processing is split up into the default template and several named 
	templates.  If you are not familiar with xslt, these templates are 
	roughly analogous to subroutines, but have much more limited 
	functionality, due to the xslt contraints.
    
        Also be aware that in PGEN (and the output xml stream), the hazard type 
        of mountain obscuration is MT_OBSC, while the desired output format 
        for a Sierra airmet is MTN OBSCN.
    
        Change Log
      
        E. Safford/SAIC	    12/04   	initial coding
        E. Safford/SAIC	    02/05	add sigmet reference header
        E. Safford/SAIC     03/05	Fix header line, change template
					  naming convention, airmet order
        E. Safford/SAIC     03/05	add GetUpdate template, use for header
        E. Safford/SAIC     07/05           rm update from header                   
        E. Safford/SAIC     07/05       make output format xml
        E. Safford/SAIC    10/05        overhaul by adding imported templates to improve code reuse & maintainability 
    -->
    
    
   <!--  
        Default template
    
        First match the root level of the document.  Process the header then
	call the named templates to process each of the smears.
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
        E. Safford/SAIC    09/05        add "NO IFR" message when no IFR but have MT_OBSC
        E. Safford/SAIC    10/05        use included NoHazardReport and TestFor templates
        E. Safford/SAIC    10/05        use OutputHeader and OutputFooter
        E. Safford/SAIC    11/05        Add outlook processing
    -->
    <xsl:template match="/">
        
        <xsl:variable name="faArea" select="//hdr/faArea"/>
        <xsl:variable name="issueTime" select="//hdr/issueTime"/>
        <xsl:variable name="untilTime" select="//hdr/untilTime"/>
        <xsl:variable name="outlookEndTime" select="//hdr/outlookEndTime"/>

            <!--  Make variable assignments  -->
 
        <xsl:variable name="numSmearIFR" select="count(//smear[hazard='IFR'])"/>
        <xsl:variable name="numSmearMT_OBSC" select="count(//smear[hazard = 'MT_OBSC'])"/>
        <xsl:variable name="numSmears" select="$numSmearIFR + $numSmearMT_OBSC"/>
        
        <xsl:variable name="numOutlookIFR" select="count(//outlook[hazard='IFR'])"/>
        <xsl:variable name="numOutlookMT_OBSC" select="count(//outlook[hazard='MT_OBSC'])"/>
        <xsl:variable name="numOutlooks" select="$numOutlookIFR + $numOutlookMT_OBSC"/>
        
        <xsl:variable name="hazardTest">
            <xsl:call-template name="GetHazards"/>
        </xsl:variable>
        
        <xsl:variable name="hazards">
            <xsl:call-template name="SetHazards">
                <xsl:with-param name="hazardList" select="$hazardTest"/>
            </xsl:call-template>
        </xsl:variable>
   
        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">IFR</xsl:with-param>
                <xsl:with-param name="haz2">MT_OBSC</xsl:with-param>
            </xsl:call-template>

        </xsl:variable>

        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>

        
        <!--   Generate the airmet bulletin -->
        <xsl:element name="airmet">
            
            <!--   Output the bulletin header -->
            <xsl:call-template name="OutputHeader">
                <xsl:with-param name="report">SIERRA</xsl:with-param>
                <xsl:with-param name="hazard" select="$hazards"/>
                <xsl:with-param name="untilTime" select="$untilTime"/>
                <xsl:with-param name="faArea" select="$faArea"/>
                <xsl:with-param name="issueTime" select="$issueTime"/>
                <xsl:with-param name="amend" select="$AMD"/>
            </xsl:call-template>
     
            <!--  If no IFR hazards, output no IFR report -->
            <xsl:if test="not($numSmearIFR > 0)">
                <xsl:call-template name="NoHazardReport">    
                    <xsl:with-param name="expectedHazard">IFR</xsl:with-param>
                </xsl:call-template>
            </xsl:if>
            
            <!--  If any hazards, output all -->
            <xsl:if test="($numSmearIFR > 0) or ($numSmearMT_OBSC > 0)">
                <xsl:call-template name="SierraAirmet">
                    <xsl:with-param name="faArea" select="$faArea"/>
                    <xsl:with-param name="issueTime" select="$issueTime"/>
                    <xsl:with-param name="untilTime" select="$untilTime"/>
                </xsl:call-template>
            </xsl:if>
                                                        
            <!--  output any outlooks -->
            <xsl:if test="$numOutlooks > 0">
                <xsl:call-template name="SierraOutlook">
                    <xsl:with-param name="outlookStart" select="$untilTime"/>
                    <xsl:with-param name="outlookEnd" select="$outlookEndTime"/>
                    <xsl:with-param name="numIFR" select="$numOutlookIFR"/>
                    <xsl:with-param name="numMT_OBSC" select="$numOutlookMT_OBSC"/>
                </xsl:call-template>
            </xsl:if>
                        
 
            <!-- add bulletin footer -->
            <xsl:call-template name="OutputFooter"/>
            
        </xsl:element>        
           
    </xsl:template>    

    
    
    <!--  
        SierraAirmet
    
        This template formats the Sierra Airmet in the proper order of IFR then MT_OBSC.  
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04    initial coding
        E. Safford/SAIC    09/05    conditionally add airmet header
        E. Safford/SAIC    10/05    use GetStatus and SetStatus
	E. Safford/SAIC	   10/05    use GetHazards, rm header & footer
    -->    
    <xsl:template name="SierraAirmet">
        <xsl:param name="faArea">{faArea}</xsl:param>
        <xsl:param name="issueTime">{issueTime}</xsl:param>
        <xsl:param name="untilTime">{untilTime}</xsl:param>

        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">IFR</xsl:with-param>
                <xsl:with-param name="haz2">MT_OBSC</xsl:with-param>
            </xsl:call-template>
            
        </xsl:variable>
        
        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="hazardTest">
                <xsl:call-template name="GetHazards"/>
        </xsl:variable>
        
        <xsl:variable name="hazards">
            <xsl:call-template name="SetHazards">
                <xsl:with-param name="hazardList" select="$hazardTest"/>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:variable name="sigmetHdr">
        	<xsl:call-template name="GetSigmetHdr"/>
        </xsl:variable>
       
        
            <!--
                check for sigmet references.  Output the sigmet header if any are 
	        found.
            -->
            <xsl:if test="string-length($sigmetHdr) > 1">
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element>
       
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="normalize-space($sigmetHdr)"/></xsl:element>
            </xsl:if> 	
            
 	
        <!--  
	    output Sierra Airmet paragraphs 
	 -->
	<xsl:call-template name="OutputSierra">
	    <xsl:with-param name="hazType">IFR</xsl:with-param>
	</xsl:call-template>

	<xsl:call-template name="OutputSierra">
	    <xsl:with-param name="hazType">MT_OBSC</xsl:with-param>
	</xsl:call-template>

    </xsl:template>

    <!--  
        SierraOutlook
    
        This template outputs the Sierra Outlooks in the proper order of IFR then MT_OBSC.
       
        Change Log
        ====== ===
        E. Safford/SAIC    11/05        initial coding
        E. Safford/SAIC    01/06        add stateList param to OutputOutlook
        E. Safford/SAIC    02/06        add outlookStart/End to OutputOutlook
        E. Safford/SAIC    02/06        fix state list in MT_OBSC outlook
        E. Safford/SAIC    02/06        fix MT_OBSC hazard name
        E. Safford/SAIC    04/06        use MakeConditionsStatement
    -->    
    <xsl:template name="SierraOutlook">
        <xsl:param name="outlookStart"></xsl:param>
        <xsl:param name="outlookEnd"></xsl:param>
        <xsl:param name="numIFR">0</xsl:param>
        <xsl:param name="numMT_OBSC">0</xsl:param>
        
        
        <xsl:variable name="numOutlooks"><xsl:value-of select="number($numIFR + $numMT_OBSC)"/></xsl:variable>
       
 
        <xsl:for-each select="//outlook[hazard='IFR']">
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard"><xsl:value-of select="hazard"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="condStatement">
                <xsl:call-template name="MakeConditionsStatement">
                    <xsl:with-param name="isSmear">0</xsl:with-param>
                    <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="fromCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="fromCondsEndg"><xsl:value-of select="fromCondsEndg"/></xsl:with-param>
                    <xsl:with-param name="condsContg"><xsl:value-of select="condsContg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="otlkCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="otlkCondsEndg"/></xsl:with-param>
                </xsl:call-template>          
            </xsl:variable>
            
            <xsl:variable name="freqSevStatement">
                <xsl:call-template name="GetFreqStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
                    <xsl:with-param name="hazard" select="hazard"/>
                    <xsl:with-param name="topBottom"><xsl:value-of select="Top_Bottom"/></xsl:with-param>
                    <xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart"><xsl:value-of select="$outlookStart"/></xsl:with-param>
                <xsl:with-param name="outlookEnd"><xsl:value-of select="$outlookEnd"/></xsl:with-param>
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="stateList"/>
                <xsl:with-param name="outlookNumber" select="position()"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="fromLine"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>                
            </xsl:call-template>
            
        </xsl:for-each>
        
        <xsl:for-each select="//outlook[hazard='MT_OBSC']">
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard"><xsl:value-of select="hazard"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="condStatement">
                <xsl:call-template name="MakeConditionsStatement">
                    <xsl:with-param name="isSmear">0</xsl:with-param>
                    <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="fromCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="fromCondsEndg"><xsl:value-of select="fromCondsEndg"/></xsl:with-param>
                    <xsl:with-param name="condsContg"><xsl:value-of select="condsContg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="otlkCondsDvlpg"/></xsl:with-param>
                    <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="otlkCondsEndg"/></xsl:with-param>
                </xsl:call-template>          
            </xsl:variable>
            
            <xsl:variable name="freqSevStatement">
                <xsl:call-template name="GetFreqStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
                    <xsl:with-param name="hazard"><xsl:value-of select="$hazardName"/></xsl:with-param>
                    <xsl:with-param name="topBottom"><xsl:value-of select="Top_Bottom"/></xsl:with-param>
                    <xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart"><xsl:value-of select="$outlookStart"/></xsl:with-param>
                <xsl:with-param name="outlookEnd"><xsl:value-of select="$outlookEnd"/></xsl:with-param>
                <xsl:with-param name="hazard"><xsl:value-of select="$hazardName"/></xsl:with-param>
                <xsl:with-param name="stateList" select="stateList"/>
                <xsl:with-param name="outlookNumber" select="position()+$numIFR"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="fromLine"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>  
            </xsl:call-template>
 
        </xsl:for-each>
        
    </xsl:template>

    
    <!--  
        OutputSierra
    
        This template outputs all the Sierra Airmet paragraphs for the given hazard type.  
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04    initial coding
        E. Safford/SAIC    08/05    Add "...UPDT" to the state list following amended and corrected hazards
        E. Safford/SAIC    11/05    rm severity from template GetFreqStatement
        E. Safford/SAIC    02/06    do not include freqSevStatement when CAN
        E. Safford/SAIC    04/06    add check to freqSevStatement length
        E. Safford/SAIC    04/06    use MakeConditionsStatement
        E. Safford/SAIC    04/06    correct bug with Status
	B. Yin/SAIC	   07/07    add tag number
	B. Yin/SAIC	   02/08    removed tag number for cancellation (added into attention line)
    -->    
    <xsl:template name="OutputSierra">
        <xsl:param name="hazType"/>

        <xsl:for-each select="//smear">
            
            <xsl:if test="hazard = $hazType">

                <xsl:variable name="hazardName">
                    <xsl:call-template name="SetHazardName">
                          <xsl:with-param name="hazard" select="hazard"/>
                    </xsl:call-template>
                </xsl:variable>
                   
                <xsl:variable name="condStatement">
                    <xsl:call-template name="MakeConditionsStatement">
                        <xsl:with-param name="isSmear">1</xsl:with-param>
                        <xsl:with-param name="fromCondsDvlpg"><xsl:value-of select="fromCondsDvlpg"/></xsl:with-param>
                        <xsl:with-param name="fromCondsEndg"><xsl:value-of select="fromCondsEndg"/></xsl:with-param>
                        <xsl:with-param name="condsContg"><xsl:value-of select="condsContg"/></xsl:with-param>
                        <xsl:with-param name="otlkCondsDvlpg"><xsl:value-of select="otlkCondsDvlpg"/></xsl:with-param>
                        <xsl:with-param name="otlkCondsEndg"><xsl:value-of select="otlkCondsEndg"/></xsl:with-param>
                    </xsl:call-template>          
                </xsl:variable>
                
                <xsl:variable name="freqSevStatement">
                	<xsl:call-template name="GetFreqStatement">
                		<xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
                		<xsl:with-param name="hazard" select="hazard"/>
                		<xsl:with-param name="topBottom"><xsl:value-of select="Top_Bottom"/></xsl:with-param>
                		<xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                		<xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                	</xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="status"><xsl:value-of select="Status"/></xsl:variable>
                
                <xsl:variable name="updateFlag">
                    <xsl:call-template name="GetUpdate">
                        <xsl:with-param name="status"><xsl:value-of select="$status"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element> 
                
                <!--  Hazard statement, state list, and update flag    -->
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>AIRMET <xsl:value-of select="$hazardName"/>...<xsl:value-of select="stateList"/>
                    <xsl:if test="number( string-length($updateFlag) ) > number( '1' )"> 
                        <xsl:value-of select="normalize-space($updateFlag)"/>
                   </xsl:if>    
                </xsl:element>
                
                <!--  From line -->
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute>FROM <xsl:value-of select="normalize-space(fromLine)"/></xsl:element>
                
                <xsl:variable name="airTag"><xsl:value-of select="airmetTag"/></xsl:variable>

                <!-- Frequency & Severity line -->
                <xsl:if test="not( contains( Status, 'CAN' )) and string-length($freqSevStatement) > 1">
                    <xsl:element name="line">
                        <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if></xsl:element>
                 </xsl:if>

                <!--  Add the attention line(s) -->
                <xsl:call-template name="GetAttentionLine">
                    <xsl:with-param name="status"><xsl:value-of select="$status"/></xsl:with-param>
                </xsl:call-template>
                
            </xsl:if>
        </xsl:for-each>

    </xsl:template>

        
    <!--
        GetHazards
        
        This template outputs the name of the hazards found in any smear in the 
        xml file.
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
	E. Safford/SAIC    10/05	rename getHazards -> GetHazards
        E. Safford/SAIC    02/06        include outlook hazards
    -->
    <xsl:template name="GetHazards">
        <xsl:for-each select="//smear">
            <xsl:value-of select="hazard"/>
        </xsl:for-each>
        
        <xsl:for-each select="//outlook">
            <xsl:value-of select="hazard"/>
        </xsl:for-each>
    </xsl:template>
    
    
    <!--
        SetHazards
    
        This template outputs the correct hazard phrase for the Sierra Airmet 
	header.  It takes as input the hazards from all the smears and outputs 
	the values in a single phrase with no repeats.  The challenge is the 
	placement of "AND".  If there are two hazard types, the AND goes 
	between them; if there are three, then the AND goes between 2 and 3 
	(there is no punctuation between 1 and 2).  
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
    -->
    <xsl:template name="SetHazards">
        <xsl:param name="hazardList"/>
        
        <xsl:choose>
            <xsl:when test="contains($hazardList, 'MT_OBSC')">IFR AND MTN OBSCN</xsl:when>
            <xsl:otherwise>IFR</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
    <!--
        GetCondStatement
    
        This template returns the "conditions" statement.  It will either be 
	the conditions followed by a period or a null string (if there are no 
        conditions).
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
    -->
    <xsl:template name="GetCondStatement">
        <xsl:param name="conds"></xsl:param>
        <xsl:if test="string-length($conds) >1"><xsl:text> </xsl:text><xsl:value-of select="$conds"/><xsl:text>.</xsl:text></xsl:if>
    </xsl:template>
    
    
    <!--
    	GetFreqStatement
    		
    	This tepmplate returns the frequency/severity/flight level/due 
	to/conditions statement.  It will return these in the correct order, 
	provided they exist in the smear.  If they do not, then a null 
	string will be returned.
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
        E. Safford/SAIC    10/05        check each component for length > 1 to avoid empty spaces.
        E. Safford/SAIC    10/05        include severity only for IFR
        E. Safford/SAIC    11/05        never include severity
        E. Safford/SAIC    04/06        modify for new conditions handling
        E. Safford/SAIC    06/06        rm final period after conditions
    -->
    <xsl:template name="GetFreqStatement">
    	<xsl:param name="frequency"/>
    	<xsl:param name="severity"/>
    	<xsl:param name="hazard"/>
    	<xsl:param name="topBottom"/>
    	<xsl:param name="dueTo"/>
    	<xsl:param name="conditions"/>
    	
    	<xsl:if test="string-length($frequency) >1 or
    		          string-length($topBottom) > 1 or
    		          string-length($dueTo) > 1" >
    	    
    	    <xsl:if test="string-length($frequency) >1">
    	        <xsl:variable name="validFrequency">
    	            
    	            <xsl:call-template name="OkToUse">
    	                <xsl:with-param name="test" select="$frequency"/>
    	            </xsl:call-template>
    	        </xsl:variable>
    	       
    	        <xsl:if test="string-length($validFrequency) > 1">
    	            <xsl:value-of select="normalize-space($validFrequency)"/>
    	        </xsl:if>
    	    </xsl:if>
    	    
    	    <xsl:if test="string-length($topBottom) > 1">
    	        <xsl:text> </xsl:text>
    	        <xsl:value-of select="normalize-space($topBottom)"/>
    	    </xsl:if>
	    
    	    <xsl:if test="string-length($dueTo) > 1">
    	        <xsl:text> </xsl:text>
    	        <xsl:value-of select="normalize-space($dueTo)"/>
    	    </xsl:if> 
               <xsl:text>.</xsl:text>
    	    
        </xsl:if>
    	
        <xsl:if test="string-length( $conditions ) > 1">
            <xsl:text> </xsl:text><xsl:value-of select="normalize-space($conditions)"/>
        </xsl:if>


    </xsl:template>


    <!--
   	GetSigmetHdr
   			
   	This template builds and returns the sigmets header statement that 
	immediately follows the airmet header.  Note that we need to check 
	the hazard here so we don't pick up any smears of types that don't 
	conform to Sierra class airmets.  (IFR is the only type within
   	Tango that can reference a sigmet.)
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
    -->
    <xsl:template name="GetSigmetHdr">

        <xsl:variable name="testSigmet">
   	    <xsl:call-template name="ListSigmetRefs"/>
   	</xsl:variable> 

   	<xsl:if test="string-length($testSigmet) > 1"> 
            ...SEE SIGMET <xsl:value-of select="$testSigmet"/>SERIES FOR POSS BLDU
	</xsl:if>

    </xsl:template>
    
    
    <!--
    	ListSigmetRefs
    		
    	This template returns any sigmets found in any smears.  The context 
	here is the smear which is selected in the calling template.  All 
	RefereToSigmet elements are selected and their values sent to output 
	by the value-of select"." statement (which selects the string 
	associated with the ReferToSigmet element).  The if test adds the AND 
	after the next to last sigmet reference if there is more than 1 
	reference.
       
        Change Log
        ====== ===
        E. Safford/SAIC    12/04        initial coding
        E. Safford/SAIC    06/05        add select for all TURB hazards and select only
                                         the unique ones
    -->
    <xsl:template name="ListSigmetRefs">

        <xsl:for-each 
           select="//smear[hazard='IFR']/ReferToSigmet[generate-id(.) = generate-id( key('SigmetKey', .)[1])]"> 
            <xsl:sort order="ascending"/>
 	    <xsl:value-of select="."/><xsl:text> </xsl:text>
 	        <xsl:if test="position() + 1 = last() and last() &gt; 1">AND </xsl:if>
 	</xsl:for-each>

    </xsl:template>


</xsl:stylesheet>
