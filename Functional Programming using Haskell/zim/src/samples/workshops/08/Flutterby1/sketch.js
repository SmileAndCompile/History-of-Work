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

function setup2() {
   createCanvas(W, H);
   for (let i = 0; i < N_FLOW; i++) {
      flowerSp[i] = createSprite(random(INSET, 2 * W - INSET), 
         random(INSET, 2 * H - INSET), flowerImg.width, 
         flowerImg.height);
      flowerSp[i].addImage("i", flowerImg);
      for (let j = 0; j < i; j++) {
         flowerSp[i].displace(flowerSp[j]);
      }
   }
   for (let i = 0; i < N_BUTT; i++) {
      butterflySp[i] = createSprite(random(INSET, 2 * W - INSET), 
         random(INSET, 2 * H - INSET), butterflyImg.width, 
         butterflyImg.height);
      butterflySp[i].addAnimation("flap", "images/butterfly00.png", 
         "images/butterfly15.png");
      butterflySp[i].friction = 0.1;
      whichFlower[i] = flowerSp[floor(random(0, N_FLOW))];
      
   }
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
