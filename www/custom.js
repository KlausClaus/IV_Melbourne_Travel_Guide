Shiny.addCustomMessageHandler('setWeatherBackground', function(weatherCondition) {
    var weatherDiv = document.getElementById('weatherBackground');
    weatherDiv.className = weatherCondition;
});

// get destination's location
function setDestination(lat, lng) {
  Shiny.setInputValue('selected_lat', lat);
  Shiny.setInputValue('selected_lng', lng);
}

/*get user location*/
function getUserLocation() {
  if (navigator.geolocation) {
    console.log("Getting user location...")
    navigator.geolocation.getCurrentPosition(function(position) {
      Shiny.setInputValue("user_coords", {
        lat: position.coords.latitude,
        lng: position.coords.longitude,
      });
    });
  } else {
    alert("Geolocation is not supported by this browser.");
  }
}

// switch To Google Map
function switchToGoogleMapTab() {
    $('a[data-value="Hidden Tab"]').tab('show');
}

$(document).ready(function() {
  // 使用标签标题选择要隐藏的标签
  var tabToHide = $("a[data-value='Hidden Tab']");
  // 隐藏标签
  tabToHide.hide();
});







