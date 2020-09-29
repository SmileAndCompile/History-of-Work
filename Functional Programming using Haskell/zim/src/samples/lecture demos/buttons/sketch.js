const BUTN_X = 100, // rect button
      BUTN_W = 80,
      BUTN_Y = 80,
      BUTN_H = 40;
      
const CBTN_X = 200, // circular button
      CBTN_Y = 200,
      CBTN_R = 60;
      


function setup() {
    createCanvas(400, 400);
}

function draw() { 
   background(230);
   if (mouseX > BUTN_X && mouseX < BUTN_X + BUTN_W &&
       mouseY > BUTN_Y && mouseY < BUTN_Y + BUTN_H) {
      fill(100);
   } else {
      fill(255);
   }
   rect(BUTN_X, BUTN_Y, BUTN_W, BUTN_H);
   let d = dist(CBTN_X, CBTN_Y, mouseX, mouseY)
   if (d < CBTN_R) {
      fill(100);
   } else {
      fill(255);
   }
   ellipse(CBTN_X, CBTN_Y, CBTN_R * 2);
}

