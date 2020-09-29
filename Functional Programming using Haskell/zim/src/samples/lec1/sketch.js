function setup() {
  createCanvas(400, 400);
  background(200, 200, 0);
}

function blob() {
    ellipse(100, 100, 100, 50);
}

function draw() {
  blob();
  ellipse(mouseX, mouseY, 20, 20);
}