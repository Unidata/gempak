function countyMapType(tileSize) {
	  this.tileSize = tileSize;
	  this.opacity = 0.2;
	}

	countyMapType.prototype.getTile = function(coord, zoom, ownerDocument) {
		  var div = ownerDocument.createElement('DIV');
		  div.innerHTML = coord;
	  

			var url = tileCacheNoPurgeURL+'county/'+zoom+'/'+coord.x+'/'+coord.y+'.png';
			  div.innerHTML = '<img src="'+url+'"/>';
			  div.style.width = this.tileSize.width + 'px';
			  div.style.height = this.tileSize.height + 'px';
			  div.style.fontSize = '10';
			  div.style.borderStyle = 'solid';
			  div.style.borderWidth = '0px';
			  div.style.borderColor = '#AAAAAA';
			  div.style.opacity = this.opacity;
			  div.id = "countyDiv";
			  div.name = "countyDiv";
			  return div;
	};
	
	
	countyMapType.prototype.toggle = function() {
		//console.log('here: '+opacityheres);
		if(this.opacity != 0.0){
		this.opacity = 0.0;
		$('div#countyDiv').css('opacity',this.opacity);
		}
		else{
			this.opacity = 0.2;
			$('div#countyDiv').css('opacity',this.opacity);
		}
		
	}