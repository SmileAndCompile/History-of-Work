// display 2 images (visible and infrared) with a handle to slide between them.

const INSET = 30, // image inset within this border
      HANDLE_W = 15, // handle dimensions
      HANDLE_H = 40;

let ir, vis; // images, both the same size
let handleX = INSET, // x coord of the slider handle
    inHandle = false; // was the mouse pressed in the handle

function preload() {
   ir = loadImage("images/infrared.jpg");
   vis = loadImage("images/visible.jpg");
}

function setup() {
   // size of canvas determined by the images
   createCanvas(ir.width + 2.0 * INSET, ir.height + 2.0 * INSET);
}

// return true iff (x, y) is inside the rectangle (rx, ry, w, h).
function pointInRect(x, y, rx, ry, w, h) {
   return rx <= x && x < rx + w && ry <= y && y < ry + h;
}

function mousePressed() {
   if (pointInRect(mouseX, mouseY,
       handleX - HANDLE_W / 2.0, height / 2.0 - HANDLE_H / 2.0, 
       HANDLE_W, HANDLE_H)) {
      inHandle = true;
   }
}

function mouseReleased() {
   inHandle = false
}

// draw the visible image, then part of the ir image depending on handeX
function drawImages() {
   image(vis, INSET, INSET);
   if (handleX > INSET) {
      image(ir, INSET, INSET, handleX - INSET, ir.height, 0, 0, handleX - INSET, ir.height);
   }
}

// track the mouse to update handleX
function moveHandle() {
   if (inHandle) {
      handleX = max(INSET, min(width - INSET, mouseX));
   }
}

// draw the handle
function drawHandle() {
   fill(230); stroke(0);
   triangle(handleX - HANDLE_W / 2.0, height / 2.0,
            handleX, height / 2.0 - HANDLE_H / 2.0,
            handleX, height / 2.0 + HANDLE_H / 2.0);
   triangle(handleX + HANDLE_W / 2.0, height / 2.0,
            handleX, height / 2.0 - HANDLE_H / 2.0,
            handleX, height / 2.0 + HANDLE_H / 2.0);
}

// draw the labels, obscured depending on handleX
function drawLabels() {
   textAlign(RIGHT); fill(255);
   text("visible", width - INSET, height - INSET + 14);
   textAlign(LEFT); 
   text("infrared", INSET, height - INSET + 14);
   fill(0); noStroke();
   if (handleX < width / 2.0) {
      rect(handleX, height - INSET, width / 4.0, INSET);
   } else {
      rect(handleX - width / 4.0, height - INSET, width / 4.0, INSET);
   }
}

function draw() {
   background(0);
   moveHandle();
   drawImages();
   drawHandle();
   drawLabels();
}
