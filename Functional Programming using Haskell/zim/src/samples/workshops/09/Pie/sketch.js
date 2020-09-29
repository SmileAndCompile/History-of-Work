// draw a pie chart using JSON data

const INSET = 40, // border around everything
      DIAMETER = 200; // of pie chart circle
      XC = INSET + DIAMETER / 2.0, YC = INSET + DIAMETER / 2.0, // centre coords
      COLOUR = ["blue", "red", "green", "yellow", "purple", 
                "orange", "pink", "brown", "black", "white"],
      LEG_X = DIAMETER + INSET * 2, LEG_Y = INSET, // legend top-left
      LEG_W = 20, LEG_H = 20, // legend colour block dimensions
      LEG_TW = 60; // legend text width

let movieData; // JSON

function preload() {
   movieData = loadJSON("data/movies.json");
}

function setup() {
   // caclulate the total count
      let total = 0;
      for (let i = 0; i < movieData.histogram.length; i++) {
         total += movieData.histogram[i].count;
      }
   // Calculate angles (adding them as new fields: startA, stopA)
      let theta = 0.0;
      for (let i = 0; i < movieData.histogram.length; i++) {
         let fraction = movieData.histogram[i].count / total;
         movieData.histogram[i].startA = theta;
         theta += fraction * TWO_PI;
         movieData.histogram[i].stopA = theta;
      }
   // prepare for drawing
      createCanvas(DIAMETER + INSET * 3 + LEG_TW, DIAMETER + INSET * 2);
}

function draw() {
   // the pie and the legend
      for (let i = 0; i < movieData.histogram.length; i++) {
         fill(COLOUR[i]);
         arc(XC, YC, DIAMETER, DIAMETER, movieData.histogram[i].startA, 
             movieData.histogram[i].stopA, PIE);
         rect(LEG_X, LEG_Y + i * LEG_H, LEG_W, LEG_H);
         fill(0); noStroke();
         text(movieData.histogram[i].type, LEG_X + LEG_W + 10, LEG_Y + i * LEG_H + 12);
         stroke(0);
      }      
}
