const HEAD_X = 40, // left of head
      HEAD_W = 120, // width of head
      HEAD_H = 160, // height of head (mouth closed)
      HEAD_R = 20, // round corner radius of head
      HAIR_H = 40, // height of hair
      CHIN_H = 40, // height of chin
      NOSE_H = 40, // height of nose
      NOSE_W = 30, // width of nose
      NOSE_X = HEAD_X + HEAD_W, // left of nose
      EYE_D = 25, // eye diameter
      EYE_X = HEAD_X + HEAD_W - 25, // eye centre x
      PUPIL_D = EYE_D / 2.0, // pupil diameter
      MOUTH_W = 80, // mouth width
      MOUTH_X = HEAD_X + HEAD_W - MOUTH_W, // mouth left
      BUN_W = 70, // width of bun
      BUN_H = 45, // height of bun
      BUN_R = 10, // round corner radius of bun
      PAT_W = 84, // patty width
      PAT_H = 7; // patty height
      NEAR = 100, // how near for the mouth to start to open
      MOUTH_H = BUN_H + 20; // maximum mouth height (gape)

let headY = 40, // top of head
    burgerX, burgerY, // coords of burger centre
    burgerVX, burgerVY, // burger velocity
    score = 0; 

// reset the burger to new random coords and velocity    
function newBurger() {
   burgerX = random(NOSE_X + PAT_W / 2.0, width - PAT_W / 2.0);
   burgerY = random(BUN_H / 2.0, height - PAT_H / 2.0);
   burgerVX = random(-2.5, -1.0);
   burgerVY = random(-2.0, 2.0);
}

function setup() {
   createCanvas(600, 400);
   newBurger();
}

function draw() {
   const NOSE_Y = headY + HAIR_H + 20, // top of nose
         MOUTH_Y = headY + HEAD_H - CHIN_H, // mouth top
         EYE_Y = NOSE_Y; // eye centre y
   // gape calculation
      let dx = burgerX - PAT_W / 2.0 - NOSE_X; // face to patty x distance
      let mouthH; // height of the mouth (gape)
      if (dx < -MOUTH_W) { // eaten!
          score++;
          newBurger();
          return;
      } else if (dx >= NEAR) {
         mouthH = 0;
      } else if (dx <= 0) {
         mouthH = MOUTH_H;
      } else {
         mouthH = MOUTH_H * (1.0 - dx / NEAR);
      }
   // collision detection
      if (burgerVX < 0.0 && dx + burgerVX < 0.0 &&
          (burgerY < MOUTH_Y + BUN_H / 2.0 || burgerY > MOUTH_Y + MOUTH_H - BUN_H / 2.0)) {
          burgerX = NOSE_X + PAT_W / 2.0;
          burgerVX = -burgerVX;
      } else if (dx < 0 && burgerVY < 0.0 && burgerY + burgerVY - BUN_H / 2.0 < MOUTH_Y) {
          burgerY = MOUTH_Y + BUN_H / 2.0;
          burgerVY = -burgerVY;
      } else if (dx < 0 && burgerVY > 0.0 && burgerY + BUN_H / 2.0 + burgerVY > MOUTH_Y + mouthH) {
          burgerY = MOUTH_Y - BUN_H / 2.0 + mouthH;
          burgerVY = -burgerVY;
      } else {
      }
      if (burgerX + PAT_W / 2.0 + burgerVX > width) {
         burgerX = width - PAT_W / 2.0;
         burgerVX = -burgerVX;
      }
      if (burgerVY < 0.0 && burgerY - BUN_H / 2.0 + burgerVY < 0) { // roof
         burgerY = BUN_H / 2.0;
         burgerVY = -burgerVY;
      }
      if (burgerVY > 0.0 && burgerY + BUN_H / 2.0 + burgerVY > height) { // roof
         burgerY = height - BUN_H / 2.0;
         burgerVY = -burgerVY;
      }
   background(220);
   // nose
      stroke(0); fill(255);
      triangle(NOSE_X, NOSE_Y, NOSE_X, NOSE_Y + NOSE_H, NOSE_X + NOSE_W, NOSE_Y + NOSE_H);
   // hair
      stroke(0); fill(0);
      rect(HEAD_X, headY, HEAD_W, HAIR_H + HEAD_R, HEAD_R);
   // chin
      stroke(0); fill(255);
      rect(HEAD_X, headY + HEAD_H - CHIN_H - HEAD_R + mouthH, HEAD_W, CHIN_H + HEAD_R, HEAD_R);
   // face
      noStroke(); fill(255);
      rect(HEAD_X, headY + HAIR_H, HEAD_W, HEAD_H - HAIR_H - CHIN_H + mouthH);
   // eye
      stroke(0); fill(255);
      ellipse(EYE_X, EYE_Y, EYE_D);
   // pupil
      let theta = atan2(burgerY - EYE_Y, burgerX - EYE_X);
      noStroke(); fill(0);
      ellipse(EYE_X + EYE_D * cos(theta) / 4.0, EYE_Y + EYE_D * sin(theta) / 4.0, PUPIL_D);
   // head outline
      noFill(); stroke(0);
      rect(HEAD_X, headY, HEAD_W, HEAD_H + mouthH, HEAD_R);
   // mouth
      fill(220); stroke(0);
      rect(MOUTH_X, MOUTH_Y, MOUTH_W + 10, mouthH);
      noStroke();
      rect(HEAD_X + HEAD_W + 1, MOUTH_Y - 10, 20, mouthH + 20);
   // burger 
      rectMode(CENTER);
         stroke(0); fill(255);
         rect(burgerX, burgerY, BUN_W, BUN_H, BUN_R);
         fill(0);
         rect(burgerX, burgerY, PAT_W, PAT_H, BUN_R);
      rectMode(CORNER);
   // move Ralph
      if (keyIsDown(DOWN_ARROW)) {
         headY++;
      } else if (keyIsDown(UP_ARROW)) {
         headY--;
      } else {
      }
   // move burger
      burgerX += burgerVX;
      burgerY += burgerVY;
   // score
      noStroke(); fill(0);
      text('burgers eaten: ' + score, width - 115, 20);
}
