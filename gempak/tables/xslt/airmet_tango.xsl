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
    <xsl:import href="make_flight_level.xsl"/>
    <xsl:import href="make_conditions.xsl"/>
    
    <xsl:output method="xml"/>

    <!--  SigmetKey is used to uniquely identify sigmet references that may
          occur in more than one airmet.  Regardless of how many times a given
          sigmet is referenced we only want a single sigmet in the text report.
     -->
    <xsl:key name="SigmetKey" 
            match="smear[hazard='TURB']/ReferToSigmet"
              use="."/>

    
    <!--
        airmet_tango.xsl
    
        This is an xslt template document.  It is to be used to process an xml 
	document that conforms to the airmet.xsd schema and produce xml output 
	containing the correct content for an Tango Airmet report.  To get the 
        final text report transform the output xml using the indent.xsl 
        stylesheet.
    
        The processing is split up into the default template and several named 
	templates.  If you are not familiar with xslt, these templates are 
	roughly analogous to subroutines, but have much more limited 
	functionality, due to the xslt contraints.
   
        Warning:  Do not use double dashes inside of the comment sections.  Xslt  
        becomes seriously unhappy over those.
 
        Change Log
        ====== ===

        E. Safford/SAIC    11/04    initial coding
        E. Safford/SAIC    11/04    add newline after FROM, add getCondStatement 
				      template
        E. Safford/SAIC    12/04    translate "SFC_WND" to "STG SFC WNDS"
        E. Safford/SAIC	   02/05    add sigmet references
        E. Safford/SAIC	   03/05    add params to NoTango function and tweak its 
	                              output
	E. Safford/SAIC    03/05    set order of airmet output, standardize
	                              template names
        E. Safford/SAIC    03/05    add update number in header
	E. Safford/SAIC    04/05    Final "LLWS" should be "LLWS EXP"
	E. Safford/SAIC    07/05    rm update number from header
        E. Safford/SAIC    07/05    make output xml using indent.xsd schema
        E. Safford/SAIC    10/05    overhaul by adding imported templates to improve code reuse & maintainability 
    -->
    
    
   <!--  
        Default template
    
        First match the root level of the document.  Process the header then
	call the named templates to process each of the smears.  This template
	is called in the process of an xslt transformation.  It then calls
	(either implicitly or explicitly) all the other templates included 
	in this file.
       
        Change Log
        ====== ===
        E. Safford/SAIC    ??/05      initial coding
        E. Safford/SAIC    10/05    call NoHazardReport and TestFor (imported templates)
        E. Safford/SAIC    10/05    include NoHazardReport if only LLWS hazard present
        E. Safford/SAIC    10/05    use OutputHeader and OutputFooter
        E. Safford/SAIC    10/05    correct SFC_WND in haz2 param to GetStatus call
        E. Safford/SAIC    12/05    rm TestForSmear, add outlook support
    -->
    <xsl:template match="/">
 

        <xsl:variable name="faArea" select="//hdr/faArea"/>
        <xsl:variable name="issueTime" select="//hdr/issueTime"/>
        <xsl:variable name="untilTime" select="//hdr/untilTime"/>
        <xsl:variable name="outlookEndTime" select="//hdr/outlookEndTime"/>
        
        <!-- Process any Tango category Airmets -->
 
        <xsl:variable name="numSmearTango" select="count(//smear[hazard='TURB'])"/>
        <xsl:variable name="numSmearSFC_WND" select="count(//smear[hazard='SFC_WND'])"/>
        <xsl:variable name="numSmearLLWS" select="count(//smear['LLWS'])"/>
        <xsl:variable name="numSmears" select="$numSmearTango +$numSmearSFC_WND + $numSmearLLWS"/>
        
        <xsl:variable name="numOutlookTURB" select="count(//outlook[hazard='TURB'])"/>
        <xsl:variable name="numOutlookSFC_WND" select="count(//outlook[hazard='SFC_WND'])"/>
        <xsl:variable name="numOutlookLLWS" select="count(//outlook[hazard='LLWS'])"/>
        <xsl:variable name="numOutlooks" select="$numOutlookTURB + $numOutlookSFC_WND + $numOutlookLLWS"/>
        
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
                <xsl:with-param name="haz1">TURB</xsl:with-param>
                <xsl:with-param name="haz2">SFC_WND</xsl:with-param>
                <xsl:with-param name="haz3">LLWS</xsl:with-param>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="AMD">
            <xsl:call-template name="SetStatus">
                <xsl:with-param name="amdTest" select="$amdTest"/>
            </xsl:call-template>
        </xsl:variable>

        
	<xsl:element name="report">	  
	    
	    <!-- Output the bulletin header -->
	    <xsl:call-template name="OutputHeader">
	        <xsl:with-param name="report">TANGO</xsl:with-param>
	        <xsl:with-param name="hazard" select="$hazards"/>
	        <xsl:with-param name="untilTime" select="$untilTime"/>
	        <xsl:with-param name="faArea" select="$faArea"/>
	        <xsl:with-param name="issueTime" select="$issueTime"/>
	        <xsl:with-param name="amend" select="$AMD"/>
	    </xsl:call-template>
	    
	    <!--  If no TURB hazards, output no TURB report -->
	    <xsl:if test="not($numSmearTango > 0)">
	        <xsl:call-template name="NoHazardReport">    
	            <xsl:with-param name="expectedHazard">TURB</xsl:with-param>
	        </xsl:call-template>
	    </xsl:if>
	    
	    <!--  If any hazards, output all -->
	    <xsl:if test="$numSmears > 0 ">
	        <xsl:call-template name="TangoAirmet">
	            <xsl:with-param name="faArea" select="$faArea"/>
	            <xsl:with-param name="issueTime" select="$issueTime"/>
	            <xsl:with-param name="untilTime" select="$untilTime"/>
	        </xsl:call-template>
	    </xsl:if>
	   
	    <!--  output any outlooks -->
	    <xsl:if test="$numOutlooks > 0">
	        <xsl:call-template name="TangoOutlook">
	            <xsl:with-param name="outlookStart" select="$untilTime"/>
	            <xsl:with-param name="outlookEnd" select="$outlookEndTime"/>
	            <xsl:with-param name="numTURB" select="$numOutlookTURB"/>
	            <xsl:with-param name="numSFC_WND" select="$numOutlookSFC_WND"/>
	            <xsl:with-param name="numLLWS" select="$numOutlookLLWS"/>
	            <xsl:with-param name="numOutlooks" select="$numOutlooks"/>
	        </xsl:call-template>
	    </xsl:if>
	    
	    <!--  Add the footer -->
	    <xsl:call-template name="OutputFooter"/>
	    
        </xsl:element>
 
    </xsl:template>    

    
    
    <!--  
        TangoAirmet
    
        This template formats the Tango Airmet.  

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05        initial coding
    -->    
    <xsl:template name="TangoAirmet">
        <xsl:param name="faArea">{faArea}</xsl:param>
        <xsl:param name="issueTime">{issueTime}</xsl:param>
        <xsl:param name="untilTime">{untilTime}</xsl:param>

        <xsl:variable name="amdTest">
            <xsl:call-template name="GetStatus">
                <xsl:with-param name="haz1">TURB</xsl:with-param>
                <xsl:with-param name="haz2">LLWS</xsl:with-param>
                <xsl:with-param name="haz3">SFC_WND</xsl:with-param>
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
            check for sigmet references.  Output the sigmet header if any are found.
        -->
	<xsl:if test="string-length($sigmetHdr) > 1">
            <xsl:element name="line">   
                <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element>

            <xsl:element name="line">   
                <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="normalize-space($sigmetHdr)"/></xsl:element>
	</xsl:if> 	
       
 
        <!--  
	    output Tango Airmets 
	-->
	<xsl:call-template name="OutputTango">
	    <xsl:with-param name="hazType">TURB</xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="OutputTango">
	    <xsl:with-param name="hazType">SFC_WND</xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="OutputTango">
	    <xsl:with-param name="hazType">LLWS</xsl:with-param>
	</xsl:call-template>

    </xsl:template>

    
    <!--  
        TangoOutlook
    
        This template outputs the Tango Outlooks in the proper order of TURB, SFC_WND, then LLWS.
       
        Change Log
        ====== ===
        E. Safford/SAIC    11/05        initial coding
        E. Safford/SAIC    01/06        add stateList param to OutputOutlook
        E. Safford/SAIC    02/06        add outlookStart/End to OutputOutlook
        E. Safford/SAIC    02/06        LLWS cannot have outlooks, add expect hazards to SFC_WND
        E. Safford/SAIC    04/06        use MakeConditionsStatement
	B. Yin/SAIC	   07/06	add isSmear parm to GetExpHazard
        E. Safford/SAIC    11/06        rm isSmear param to GetExpHazard, fix bug in conds wording
    -->    
    <xsl:template name="TangoOutlook">
        <xsl:param name="outlookStart"></xsl:param>
        <xsl:param name="outlookEnd"></xsl:param>
        <xsl:param name="numTURB">0</xsl:param>
        <xsl:param name="numSFC_WND">0</xsl:param>
        <xsl:param name="numLLWS">0</xsl:param>
        <xsl:param name="numOutlooks">0</xsl:param>
        
        <!--   output all TURB outlooks  -->
        <xsl:for-each select="//outlook[hazard='TURB']">    
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard">TURB</xsl:with-param>
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
                <xsl:call-template name="GetFreqSevStatement">
                    <xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
                    <xsl:with-param name="severity"><xsl:value-of select="Severity"/></xsl:with-param>
                    <xsl:with-param name="hazard" select="$hazardName"/>
                    <xsl:with-param name="top" select="Top"/>
                    <xsl:with-param name="base" select="Base"/>
                    <xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart" select="$outlookStart"/>
                <xsl:with-param name="outlookEnd" select="$outlookEnd"/>            
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="stateList"/>
                <xsl:with-param name="outlookNumber" select="position()"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="fromLine"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>
            </xsl:call-template>
            
        </xsl:for-each>
                
        <!--   output all SFC WND outlooks  -->
        <xsl:for-each select="//outlook[hazard='SFC_WND']">
      
            <xsl:variable name="hazardName">
                <xsl:call-template name="SetHazardName">
                    <xsl:with-param name="hazard">SFC_WND</xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            
            <xsl:variable name="expHazard">
                <xsl:call-template name="GetExpHazard">
                    <xsl:with-param name="haz" select="hazard"/>
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
                <xsl:call-template name="GetFreqSevStatement">                
                    <xsl:with-param name="expectedHaz" select="$expHazard"></xsl:with-param>
                    <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
          
            <xsl:call-template name="OutputOutlook">
                <xsl:with-param name="outlookStart" select="$outlookStart"/>
                <xsl:with-param name="outlookEnd" select="$outlookEnd"/>     
                <xsl:with-param name="hazard" select="$hazardName"/>
                <xsl:with-param name="stateList" select="stateList"/>
                <xsl:with-param name="outlookNumber" select="position() + $numTURB"/>
                <xsl:with-param name="totalOutlooks" select="$numOutlooks"/>
                <xsl:with-param name="boundedBy" select="fromLine"/>
                <xsl:with-param name="freqSevStatement" select="$freqSevStatement"/>
            </xsl:call-template>
            
        </xsl:for-each>
        
    </xsl:template>
    
    
    <!-- 
        OutputTango

	This template outputs all the airmet paragraphs for a given type of hazard.

        Change Log
        ====== ===
        E. Safford/SAIC    06/05    Add call to GetFreqSevStatement
        E. Safford/SAIC    08/05    Add "...UPDT" to the state list following amended and corrected hazards
        E. Safford/SAIC    10/05    use TranslateHazard
        E. Safford/SAIC    11/05    rm TranslateHazard, use SetHazardName
        E. Safford/SAIC    01/06    use Base & Top, not Top_Bottom
        E. Safford/SAIC    02/06    no freq/sev/conds (line 3) for CAN airmets
        E. Safford/SAIC    04/06    use MakeConditionsStatement
	B. Yin/SAIC	   07/06    add isSmear parm to GetExpHazard
        E. Safford/SAIc    11/06    rm isSmearparam to GetExpHazard
	B. Yin/SAIC	   07/07    add tag number
        B. Yin/SAIC        02/08    removed tag number for cancellation (added into attention line)
    -->
    <xsl:template name="OutputTango"> 
        <xsl:param name="hazType"/>


        <xsl:for-each select="//smear">
            

            <xsl:if test="hazard = $hazType">
                
                <xsl:variable name="hazardName">
                    <xsl:call-template name="SetHazardName">
                        <xsl:with-param name="hazard"><xsl:value-of select="$hazType"/></xsl:with-param>
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
                
                <xsl:variable name="expHazard">
                    <xsl:call-template name="GetExpHazard">
                        <xsl:with-param name="haz" select="hazard"/>
                    </xsl:call-template>
                </xsl:variable>

                <xsl:variable name="freqSevStatement">
                    <xsl:call-template name="GetFreqSevStatement">
           	        <xsl:with-param name="frequency"><xsl:value-of select="Frequency"/></xsl:with-param>
           	        <xsl:with-param name="severity"><xsl:value-of select="Severity"/></xsl:with-param>
           	        <xsl:with-param name="hazard" select="$hazardName"/>
                              <xsl:with-param name="top" select="Top"/>
                              <xsl:with-param name="base" select="Base"/>
           	        <xsl:with-param name="dueTo"><xsl:value-of select="DUE_TO"/></xsl:with-param>
                              <xsl:with-param name="expectedHaz" select="$expHazard"></xsl:with-param>
           	        <xsl:with-param name="conditions"><xsl:value-of select="$condStatement"/></xsl:with-param>
      	            </xsl:call-template>
                </xsl:variable>

                <xsl:variable name="status"><xsl:value-of select="Status"/></xsl:variable>
      
                <xsl:variable name="airmetStatement">
                    <xsl:call-template name="GetAirmetStatement">
                        <xsl:with-param name="haz"><xsl:value-of select="$hazardName"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="fromLineStatement">
                    <xsl:call-template name="GetFromLineStatement">
                        <xsl:with-param name="haz" select="hazard"/>
                    </xsl:call-template>
                </xsl:variable>
                
                <xsl:variable name="updateFlag">
                    <xsl:call-template name="GetUpdate">
                        <xsl:with-param name="status"><xsl:value-of select="$status"/></xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
           
                <!-- 
                        Output begins here
                -->
                <xsl:element name="line">   
                    <xsl:attribute name="indent">0</xsl:attribute>.</xsl:element>
             	    
                <!--  Hazard statement, state list, and update flag    -->
                <xsl:element name="line">   
                    <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="$airmetStatement"/>...<xsl:value-of select="normalize-space(stateList)"/>
                    <xsl:if test="string-length($updateFlag) > 1">
                        <xsl:value-of select="normalize-space($updateFlag)"/>
                    </xsl:if>
                </xsl:element>

                <!-- From line -->
                <xsl:element name="line">
                    <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="$fromLineStatement"/><xsl:text> </xsl:text><xsl:value-of select="normalize-space(fromLine)"/></xsl:element>
                
                <!-- Frequency & Severity line -->
                <xsl:variable name="airTag"><xsl:value-of select="airmetTag"/></xsl:variable>

                <xsl:if test="not( contains( Status, 'CAN' ))">
                    <xsl:if test="string-length($freqSevStatement) > 1">
                        <xsl:element name="line">
                            <xsl:attribute name="indent">0</xsl:attribute><xsl:value-of select="normalize-space($freqSevStatement)"/><xsl:if test="string-length($airTag) > 1"><xsl:text> </xsl:text><xsl:value-of select="normalize-space($airTag)"/>.</xsl:if></xsl:element>
                    </xsl:if>
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
            
        Output all hazards found in every smear.  This will include non-Tango class
        hazards too, if any are in the xml file.

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05        initial coding
        E. Safford/SAIC    02/06    include outlook hazards

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
    
        This template outputs the correct hazard phrase for the Tango Airmet 
	header.  It takes as input the hazards from all the smears and outputs 
	the values in a single phrase with no repeats.  The challenge is the 
	placement of "AND".  If there are two hazard types, the AND goes 
	between them; if there are three, then the AND goes between 2 and 3 
	(there is no punctuation between 1 and 2).  

	Note that this is now (12/04) being also used to translate the 
	nmap2 phrase of "SFC_WND" into the airmet phrase of "STG SFC WNDS".

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05     initial coding
        E. Safford/SAIC    06/05    when all 3 hazards are included truncate
                                          do not include "STG" in surface winds 
        E. Safford/SAIC    10/05    always include TURB in the hazard list
        E. Safford/SAIC    12/05    fix default TURB
    -->
    <xsl:template name="SetHazards">
        <xsl:param name="hazardList"/>
        
        <xsl:choose>
            <xsl:when test="contains($hazardList, 'TURB') and contains($hazardList, 'SFC_WND') and contains( $hazardList, 'LLWS')">TURB STG WNDS AND LLWS</xsl:when>
            <xsl:when test="contains($hazardList, 'TURB') and contains($hazardList, 'SFC_WND')">TURB AND STG SFC WNDS</xsl:when>
            <xsl:when test="contains($hazardList, 'TURB') and contains($hazardList, 'LLWS')">TURB AND LLWS</xsl:when>
            <xsl:when test="contains($hazardList, 'SFC_WND') and contains($hazardList, 'LLWS')">TURB STG WNDS AND LLWS</xsl:when>
            <xsl:when test="contains($hazardList, 'SFC_WND')">TURB AND STG SFC WNDS</xsl:when>
            <xsl:when test="contains($hazardList, 'LLWS')">TURB AND LLWS</xsl:when>
            <xsl:otherwise>TURB</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
    <!--
        GetCondStatement
    
        This template returns the "conditions" statement.  It will either be the conditions followed by a 
        period or a null string (if there are no conditions).

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05        initial coding
    -->
    <xsl:template name="GetCondStatement">
        <xsl:param name="conds"></xsl:param>
        <xsl:if test="string-length($conds) >1">
            <xsl:text> </xsl:text><xsl:value-of select="$conds"/><xsl:text>.</xsl:text>
        </xsl:if>
    </xsl:template>
    
   
   <!--
   	GetSigmetHdr
   		
   	This template builds and returns the sigmets header statement that immediately follows 
   	the airmet header.  Note that we need to check the hazard here so we don't pick up any 
   	smears of types that don't conform to Tango class airmets.  (TURB is the only type within
   	Tango that can reference a sigmet.)

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05        initial coding
        E. Safford/SAIC    06/05        remove select; use existing context
   -->
   <xsl:template name="GetSigmetHdr">
       <xsl:variable name="testSigmet">
           <xsl:call-template name="ListSigmetRefs"/>
       </xsl:variable> 

       <xsl:if test="string-length($testSigmet) > 1"> 
           ...SEE SIGMET <xsl:value-of select="$testSigmet"/>SERIES FOR POSS SEV TURB
       </xsl:if>
   </xsl:template>
    
    
    <!--
    	ListSigmetRefs
    		
    	This template returns any sigmets found in any smears.  The context here 
        is the smear which is selected in the calling template.  All RefereToSigmet 
    	elements are selected and their values sent to output by the value-of 
    	select"." statement (which selects the string associated with the ReferToSigmet
    	element).  The if test adds the AND after the next to last sigmet reference 
    	if there is more than 1 reference.

        This template uses the SigmetKey key and the Muenchian sort algorithm to select
        only the unique ReferToSigmet values, not all ReferToSigmet values. 

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05        initial coding
        E. Safford/SAIC    06/05        add select for all TURB hazards and select only
                                         the unique ones
    -->
    <xsl:template name="ListSigmetRefs">

        <xsl:for-each
           select="//smear[hazard='TURB']/ReferToSigmet[generate-id(.) = generate-id( key('SigmetKey', .)[1])]"> 
            <xsl:sort order="ascending"/> 

            <!--  
                output the ReferToSigmet and add on "AND " if this is
                the next to last node to be processed.
             -->
            <xsl:value-of select="."/><xsl:text> </xsl:text>
                <xsl:if test="position() + 1 = last() and last() &gt; 1">AND </xsl:if>

 	</xsl:for-each>

    </xsl:template>

 
   <!--
   	GetExpHazard
   		
   	This template returns the surface wind expected line if the hazard type is 
   	is "SFC_WND".

        Change Log
        ====== ===
        E. Safford/SAIC    ??/05    initial coding
        E. Safford/SAIC    ??/05    change LLWS and SFC WND exp statement
        E. Safford/SAIC    05/06    rm periods from output
	B. Yin/SAIC	   07/06    add isSmear parm 
       E. Safford/SAIC    11/06    rm isSmear param
   -->
   <xsl:template name="GetExpHazard">
       <xsl:param name="haz">{hazard}</xsl:param>
           <xsl:choose>
               <xsl:when test="contains($haz, 'SFC_WND')">SUSTAINED SURFACE WINDS GTR THAN 30KT EXP</xsl:when>
           </xsl:choose>
           <xsl:choose>
               <xsl:when test="contains($haz, 'LLWS')">LLWS EXP</xsl:when>
           </xsl:choose>
       
   </xsl:template>


   <!--  
            GetAirmetStatement
    
            This template returns either "AIRMET [hazard type]" or "LLWS POTENTIAL".
    
        Change Log
        =======
        E. Safford/SAIC    07/05    intial coding
    -->
    <xsl:template name="GetAirmetStatement">
        <xsl:param name="haz">{hazard}</xsl:param>
            <xsl:choose>
                <xsl:when test="contains($haz, 'LLWS')">LLWS POTENTIAL</xsl:when>
                <xsl:otherwise>AIRMET <xsl:value-of select="$haz"/></xsl:otherwise>
            </xsl:choose>        
    </xsl:template>
    
    
    <!-- 
        GetFromLineStatement
    
        This template returns either "FROM" or "BOUNDED BY" for LLWS.
    
        Change Log
        =======
        E. Safford/SAIC    07/05    initial coding
    -->
    <xsl:template name="GetFromLineStatement">
        <xsl:param name="haz">{hazard}</xsl:param>
            <xsl:choose>
                <xsl:when test="contains($haz, 'LLWS')">BOUNDED BY</xsl:when>
                <xsl:otherwise>FROM</xsl:otherwise>
            </xsl:choose>
    </xsl:template>
    
    
   <!--
    	GetFreqSevStatement
    		
    	This tepmplate returns the frequency/severity/flight level/due 
	to/conditions statement.  It will return these in the correct order, 
	provided they exist in the smear.  If they do not, then a null string 
	will be returned.

        Change Log
        ====== ===
        E. Safford/SAIC    06/05    initial coding
        E. Safford/SAIC    10/05    add strlen check on Freq, normalize space
        E. Safford/SAIC    10/05    include severity and hazard only for TURB    
        E. Safford/SAIC    01/06    use MakeFlightLevel
        E. Safford/SAIC    04/06    add expectedHaz parameter
        E. Safford/SAIC    04/06    add period before condtions.
        E. Safford/SAIC    05/06    re-arrange period placement.
        E. Safford/SAIC    06/06    rm extra period after conditions
    -->
    <xsl:template name="GetFreqSevStatement">
    	<xsl:param name="frequency"/>
    	<xsl:param name="severity"/>
    	<xsl:param name="hazard"/>
           <xsl:param name="base"/>
           <xsl:param name="top"/>
    	<xsl:param name="dueTo"/>
           <xsl:param name="expectedHaz"/>
    	<xsl:param name="conditions"/>

	<xsl:if test="string-length($frequency) >1 or
    		          string-length($severity) > 1 or
    		          string-length($top) > 1 or
    		          string-length($dueTo) > 1 or
	                     string-length($expectedHaz) > 1 or
    		          string-length($conditions) > 1" >

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
	 	    
	 <xsl:variable name="flightLevelStatement">
	     <xsl:if test="contains( $hazard, 'TURB')">
	         <xsl:call-template name="MakeFlightLevel">
	             <xsl:with-param name="base" select="$base"/>
	             <xsl:with-param name="top" select="$top"/>
	         </xsl:call-template>
	     </xsl:if>
	 </xsl:variable>   
	    
            <xsl:if test="string-length($severity)>1 and contains($hazard, 'TURB')">
                <xsl:text> </xsl:text>
                <xsl:value-of select="$severity"/>
            </xsl:if>

            <xsl:if test="string-length($hazard) > 1 and contains($hazard, 'TURB')">
                <xsl:text> </xsl:text>
                <xsl:value-of select="$hazard"/>
            </xsl:if>

            <xsl:if test="string-length($flightLevelStatement)>1" >
                <xsl:text> </xsl:text>
                <xsl:value-of select="$flightLevelStatement"/>
            </xsl:if>

            <xsl:if test="string-length($dueTo)>1" >
                <xsl:text> </xsl:text>
                <text>DUE TO </text><xsl:value-of select="$dueTo"/>
            </xsl:if>
	    
            <xsl:if test="string-length($frequency) >1 or
	        string-length($severity) > 1 or
	        string-length($top) > 1 or
	        string-length($dueTo) > 1" >
	     <xsl:text>. </xsl:text>       
            </xsl:if>
	    
	 <xsl:if test="string-length($expectedHaz) > 1">
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="$expectedHaz"/>
	     <xsl:text>. </xsl:text>
	 </xsl:if>	    	    	 
	    	        
            <xsl:if test="string-length($conditions)>1" >
                <xsl:value-of select="$conditions"/>             
            </xsl:if>	            

        </xsl:if>

    </xsl:template>
    
</xsl:stylesheet>
