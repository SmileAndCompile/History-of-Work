const W = 500; // canvas width

let shelters, // geo data
    minLon, maxLon, minLat, maxLat, // ranges
    h; // computed canvas height

function preload() {
   shelters = loadJSON("data/shelters.json");
}

function setup() {
   minLon = shelters.features[0].geometry.coordinates[0];
   maxLon = shelters.features[0].geometry.coordinates[0];
   minLat = shelters.features[0].geometry.coordinates[1];
   maxLat = shelters.features[0].geometry.coordinates[1];
   for (let i = 1; i < shelters.features.length; i++) {
      minLon = min(minLon, shelters.features[i].geometry.coordinates[0]);
      maxLon = max(maxLon, shelters.features[i].geometry.coordinates[0]);
      minLat = min(minLat, shelters.features[i].geometry.coordinates[1]);
      maxLat = max(maxLat, shelters.features[i].geometry.coordinates[1]);
   }
   h = 500 * (maxLat - minLat) / (maxLon - minLon) * 60 / 53;
   createCanvas(W, h);
}

function draw() {
   background(200);
   for (let i = 0; i < shelters.features.length; i++) {
      plot(shelters.features[i].geometry.coordinates[0],
           shelters.features[i].geometry.coordinates[1]);
   }
}

// mark one point
function plot(lon, lat) {
   let x = (lon - minLon) / (maxLon - minLon) * W;
   let y = h - (lat - minLat) / (maxLat - minLat) * h;
   ellipse(x, y, 3, 3);
}
