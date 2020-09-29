function setup() {
   createCanvas(400, 400);
}

function draw() {
   background(200);
   noFill();
   ellipse(mouseX, mouseY, 20);
   line(mouseX - 20, mouseY - 20, mouseX - 2.5, mouseY - 3.5);
   line(mouseX + 20, mouseY + 20, mouseX + 2.5, mouseY + 3.5);
   line(mouseX - 20, mouseY + 20, mouseX - 2.5, mouseY + 3.5);
   line(mouseX + 20, mouseY - 20, mouseX + 2.5, mouseY - 3.5);
   textAlign(LEFT, TOP);
   fill('black')
   text("(" + mouseX + ", " + mouseY + ")", mouseX + 21, mouseY + 21);
}
