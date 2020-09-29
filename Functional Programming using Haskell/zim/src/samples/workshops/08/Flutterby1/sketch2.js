// Move one butterfly sprite with the arrow keys.

const W = 800, H = 500, // screen dimensions
      INSET = 150; // constraint on random butterflySpr initial pacement

let butterflyImg; // one image
let butterflySpr; // the sprite

function preload() {
   butterflyImg = loadImage("images/butterfly00.png");
}

function setup() {
   createCanvas(W, H);
   butterflySpr = createSprite(random(INSET, W - INSET), random(INSET, H - INSET), 
      butterflyImg.width, butterflyImg.height);
   butterflySpr.addAnimation("flap", "images/butterfly00.png", 
      "images/butterfly15.png")
}



function draw() {
   // draw in current position
      background(200);
      drawSprites();
   // update the position
      if (keyIsDown(LEFT_ARROW)) {
         butterflySpr.position.x -= 1;
      }
      if (keyIsDown(RIGHT_ARROW)) {
         butterflySpr.position.x += 1;
      }
      if (keyIsDown(UP_ARROW)) {
         butterflySpr.position.y -= 1;
      }
      if (keyIsDown(DOWN_ARROW)) {
         butterflySpr.position.y += 1;
      }
}
