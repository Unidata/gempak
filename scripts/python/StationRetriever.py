import os
import sys
from awips import ThriftClient
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gempak.request import GetStationsRequest


class StationRetriever:
    """ Retrieves all requested stations """

    def __init__(self,server,pluginName):
         self.pluginName = pluginName
         self.outdir = os.getcwd()
         self.host = os.getenv("DEFAULT_HOST", server)
         self.port = os.getenv("DEFAULT_PORT", "9581")
         self.client = ThriftClient.ThriftClient(self.host, self.port)

    def getStations(self):
         """ Sends ThriftClient request and writes out received files."""
         req = GetStationsRequest()
         req.setPluginName(self.pluginName)
         resp = self.client.sendRequest(req)

	 stns = []
	 for item in resp:
	     str = '{:<8}'.format(item.getStationId())

	     if sys.byteorder == 'little':
		 stnid = (ord(str[3]) << 24) + (ord(str[2]) << 16) + (ord(str[1]) << 8) + ord(str[0])
		 stnid2 = (ord(str[7]) << 24) + (ord(str[6]) << 16) + (ord(str[5]) << 8) + ord(str[4])
	     else:
		 stnid = (ord(str[0]) << 24) + (ord(str[1]) << 16) + (ord(str[2]) << 8) + ord(str[3])
		 stnid2 = (ord(str[4]) << 24) + (ord(str[5]) << 16) + (ord(str[6]) << 8) + ord(str[7])

	     if item.getState() == None:
		 str = '    '
	     else:
		 str = '{:<4}'.format(item.getState())

	     if sys.byteorder == 'little':
		 state = (ord(str[3]) << 24) + (ord(str[2]) << 16) + (ord(str[1]) << 8) + ord(str[0])
	     else:
		 state = (ord(str[0]) << 24) + (ord(str[1]) << 16) + (ord(str[2]) << 8) + ord(str[3])

	     str = '{:<4}'.format(item.getCountry())
	     if sys.byteorder == 'little':
		 cntry = (ord(str[3]) << 24) + (ord(str[2]) << 16) + (ord(str[1]) << 8) + ord(str[0])
	     else:
		 cntry = (ord(str[0]) << 24) + (ord(str[1]) << 16) + (ord(str[2]) << 8) + ord(str[3])

	     stns.append(9999)
	     stns.append(stnid)
	     stns.append(item.getWmoIndex())
	     stns.append(int(item.getLatitude()*100))
	     stns.append(int(item.getLongitude()*100))
	     stns.append(int(item.getElevation()))
	     stns.append(state)
	     stns.append(cntry)
	     stns.append(stnid2)
	     stns.append(0)

         return stns
             
def getstations(server,table,key,dummy,dummy2):
    sr = StationRetriever(server,table)
    return sr.getStations()

# This is the standard boilerplate that runs this script as a main
if __name__ == '__main__':
    # Run Test
    srv = 'nco-lw-msmith'
    #srv = 'nco-lw-sgilbert'
    key = '-'

    print('OBS - METAR')
    tbl = 'obs'
    print(getstations(srv,tbl,key))
    
    print('SFCOBS - SYNOP'
    tbl = 'sfcobs'
    print(getstations(srv,tbl,key))
