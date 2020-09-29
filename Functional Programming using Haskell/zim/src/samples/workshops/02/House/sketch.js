function setup() {
  createCanvas(400, 400);
}

function draw() {
   background(220, 220, 255);
   // wall
      fill (255);
      rect(100, 100, 140, 100);
   // chimney
      rect(200, 60, 10, 30)
   // roof
      triangle(90, 100, 250, 100, 170, 60);
   // door
      rect(185, 140, 30, 60);
      ellipse(190, 170, 5, 5)
   // window
      rect(120, 130, 50, 50);
      line(120, 155, 170, 155);
      line(145, 130, 145, 180);
}
