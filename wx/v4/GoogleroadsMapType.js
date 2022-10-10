function GoogleroadsMapType(tileSize) {
	  this.tileSize = tileSize;
	  this.opacity = 1.0;
	}

	GoogleroadsMapType.prototype.getTile = function(coord, zoom, ownerDocument) {
		  var div = ownerDocument.createElement('DIV');
		  div.innerHTML = coord;
	  

		var url = 'http://mt1.google.com/vt/lyrs=h@132&hl=en&s=sphericalMercator&z='+zoom+'&x='+coord.x+'&y='+coord.y;
		//var url = tileCacheURL+''+this.rid+'-'+this.pid+'-'+this.radarTimeHere+'/'+zoom+'/'+coord.x+'/'+coord.y+'.png';
		  div.innerHTML = '<img src="'+url+'"/>';
		  div.style.width = this.tileSize.width + 'px';
		  div.style.height = this.tileSize.height + 'px';
		  div.style.fontSize = '10';
		  div.style.borderStyle = 'solid';
		  div.style.borderWidth = '0px';
		  div.style.borderColor = '#AAAAAA';
		  div.style.opacity = this.opacity;
		  div.id = "googleRoadsDiv";
		  div.name = "googleRoadsDiv";
		  return div;
	};
	
	
	GoogleroadsMapType.prototype.toggle = function() {
		//console.log('here: '+opacityheres);
		if(this.opacity != 0.0){
		this.opacity = 0.0;
		$('div#googleRoadsDiv').css('opacity',this.opacity);
		}
		else{
			this.opacity = 1.0;
			$('div#googleRoadsDiv').css('opacity',this.opacity);
		}
		
	}