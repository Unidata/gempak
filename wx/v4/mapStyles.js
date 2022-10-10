   // Create an array of styles.
    var lightStyle = [
      {
        featureType: "all",
        stylers: [
                  { saturation: -80 }
        ]
      },{
	      featureType: "water",
	      stylers: [
	                { saturation: 0 }
	      ]
      },
      {
	      featureType: "road",
	      stylers: [
	                { visibility: "simplified" }
	      ]
      },
      {
	      featureType: "landscape",
	      stylers: [
	                { visibility: "off" }
	      ]
      },
      {
	      featureType: "poi",
	      stylers: [
	                { visibility: "off" }
	      ]
      },
      {
	      featureType: "transit",
	      stylers: [
	                { visibility: "off" }
	      ]
      }
    ];
    var lightMapType = new google.maps.StyledMapType(lightStyle,
    	    {name: "Light"});
    // Create an array of styles.

    
    
    // Create an array of styles.
    var darkStyle = [
      {
        featureType: "all",
        stylers: [
                  { saturation: 0, lightness: -80 }
        ]
      },{
	      featureType: "water",
	      stylers: [
	                { saturation: 0 }
	      ]
      },
      {
	      featureType: "road",
	      stylers: [
	                { visibility: "simplified" }
	      ]
      },
      {
	      featureType: "landscape",
	      stylers: [
	                { visibility: "off" }
	      ]
      },
      {
	      featureType: "poi",
	      stylers: [
	                { visibility: "off" }
	      ]
      },
      {
	      featureType: "transit",
	      stylers: [
	                { visibility: "off" }
	      ]
      }
    ];
    var darkMapType = new google.maps.StyledMapType(darkStyle,
    	    {name: "Dark"});
    // Create an array of styles.

    
    