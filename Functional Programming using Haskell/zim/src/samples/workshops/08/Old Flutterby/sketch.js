// Move an animated butterfly around.
// Andrew Rock, S1234567890000
// CC  assignment 2

const W = 800, H = 450; // screen dimensions

let butterfly0; // one image
let butterfly; // the sprite

function preload() {
    butterfly0 = loadImage("../../images/butterfly/butterfly00.png");
}

function setup() {
   createCanvas(W, H);
   butterfly = createSprite(random(200, 600), random(150, 300), 
      butterfly0.width, butterfly0.height);
   butterfly.addAnimation("flap", "../../images/butterfly/butterfly00.png", 
      "../../images/butterfly/butterfly15.png")
}

function draw() {
   // draw in current position
      background(200);
      drawSprites();
   // update the position
      if (keyIsDown(LEFT_ARROW)) {
         butterfly.position.x -= 1;
      }
      if (keyIsDown(RIGHT_ARROW)) {
         butterfly.position.x += 1;
      }
      if (keyIsDown(UP_ARROW)) {
         butterfly.position.y -= 1;
      }
      if (keyIsDown(DOWN_ARROW)) {
         butterfly.position.y += 1;
      }
}
