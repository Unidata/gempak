function RadarMapType(tileSize, radarTimeHere, num, rid, pid) {
	  this.tileSize = tileSize;
	  this.radarTimeHere = radarTimeHere;
	  this.num = num;
	  this.rid = rid;
	  this.pid = pid;
	}

	RadarMapType.prototype.getTile = function(coord, zoom, ownerDocument) {
		  var div = ownerDocument.createElement('DIV');
		  div.innerHTML = coord;
	  
		var url = tileCacheURL+'ridge::'+this.rid+'-'+this.pid+'-'+this.radarTimeHere+'/'+zoom+'/'+coord.x+'/'+coord.y+'.png';
		  div.innerHTML = '<img src="'+url+'"/>';
		  div.style.width = this.tileSize.width + 'px';
		  div.style.height = this.tileSize.height + 'px';
		  div.style.fontSize = '10';
		  div.style.borderStyle = 'solid';
		  div.style.borderWidth = '0px';
		  div.style.borderColor = '#AAAAAA';
		  if(this.num == 0){
			  div.style.opacity = radarOpacity;
			 
		  }
		  else{
			  div.style.opacity = '0.0';
		  }
		  div.id = "radarDiv"+this.radarTimeHere;
		  div.name = "radarDiv"+this.radarTimeHere;
		  return div;
	};