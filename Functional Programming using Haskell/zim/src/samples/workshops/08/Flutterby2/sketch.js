// Animate butterflies visiting random flowers.

const SCALE = 0.5, // overall scale factor
      W = 800, H = 500, // screen dimensions
      N_FLOW = 20, // number of flowers
      N_BUTT = 3, // number of flowers
      INSET = 150; // constraint on random sprite placement

let flowerImg, // one flower image
    butterflyImg, // one butterfly image
    flowerSp = Array(N_FLOW), // flower sprites
    butterflySp = Array(N_BUTT), // the butterfly sprites
    whichFlower = Array(N_BUTT); // which flower is each butterfly attracted to

function preload() {
   butterflyImg = loadImage("images/butterfly00.png");
   flowerImg = loadImage("images/flower.png");
}

function setup() {
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
      scale(SCALE);
      drawSprites();
   // update the position
      for (let i = 0; i < N_BUTT; i++) {
         if (random() < 0.002) {
            whichFlower[i] = flowerSp[floor(random(0, N_FLOW))];         
         }
         butterflySp[i].attractionPoint(0.2, whichFlower[i].position.x, 
            whichFlower[i].position.y);
         for (let j = 0; j < i; j++) {
            butterflySp[i].bounce(butterflySp[j])
         }
      }
}
