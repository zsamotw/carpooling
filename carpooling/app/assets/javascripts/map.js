function addMapLayer(name) {
	map = new OpenLayers.Map(name);
	map.addLayer(new OpenLayers.Layer.OSM());
	return map;
}

function setLonLat(lon, lat) {

	var lonLat = new OpenLayers.LonLat(lon, lat)
                 .transform(
                 new OpenLayers.Projection("EPSG:4326"), // transform
																						// from
																						// WGS
																						// 1984
                 map.getProjectionObject() // to
																			// Spherical
																			// Mercator
																			// Projection
                );
	return lonLat;
	
}

function setCenterAndZoom(lon,lat, map) {
	var lonLat = setLonLat(lon, lat);
                 
	var zoom=12;
	map.setCenter (lonLat, zoom);
}

function addMarkersLayer(map) {
	var markers = new OpenLayers.Layer.Markers("Markers");
	map.addLayer(markers);
	return markers;
}

function setMarker(lon, lat, markers, popupText) {
	var lonLat = setLonLat(lon, lat); 
                 
    markers.addMarker(new OpenLayers.Marker(lonLat));
                 	
    var popup = new OpenLayers.Popup("popup", lonLat,
                new OpenLayers.Size(200,20),
                popupText,
                 true);
    popup.show;
    map.addPopup(popup);
}


