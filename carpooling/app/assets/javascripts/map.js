function addMap(lat, lon, id) {
	  return L.map(id).setView([lat,lon], 11);
}
function setMap(map) {
		L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
				maxZoom: 18,
				attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
					  '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
					  'Imagery © <a href="http://mapbox.com">Mapbox</a>',
				id: 'mapbox.streets'
		}).addTo(map);
}

function setKindergartenPopupContent(name, street, city) {
	  return  'Kindergarten: ' + name + '<br>' + street + ' in ' + city 
}

function addKindergartenMarker(lat, lon, content, map) {
	  var marker = L.marker([lat, lon]).addTo(map);
	  marker.bindPopup(content).openPopup();
}

function setUserPopupContent(name, surname, street, city, seats, email) {
	  var content = name + ' ' + surname + '<br>' + street + ' in ' + city + '<br>' + seats + ' free seats'
	  return content;
}

function setUserPopupContentWithRequest(name, surname, street, city, seats, email) {
	var content = name + ' ' + surname + '<br>' + street + ' in ' + city + '<br>' + seats + ' free seats' + '<br>' + '<a href="/sendrequest/' + email+ '">send request</a>'
	  return content;
}

function addUserPopUp(lat, lon, content, map, color) {
	  var circle = L.circle([lat, lon], {
	      color: color,
	      fillColor: '#f03',
	      fillOpacity: 0.5,
	      radius: 500
	  }).addTo(map);
	  circle.bindPopup(content);
}
