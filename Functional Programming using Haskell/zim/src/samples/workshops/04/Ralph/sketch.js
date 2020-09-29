const HEAD_X = 40, // left of head
      HEAD_Y = 40, // top of head
      HEAD_W = 120, // width of head
      HEAD_H = 160, // height of head (mouth closed)
      HEAD_R = 20, // round corner radius of head
      HAIR_H = 40, // height of hair
      CHIN_H = 40, // height of chin
      NOSE_H = 40, // height of nose
      NOSE_W = 30, // width of nose
      NOSE_X = HEAD_X + HEAD_W, // left of nose
      NOSE_Y = HEAD_Y + HAIR_H + 20, // top of nose
      EYE_D = 25, // eye diameter
      EYE_X = HEAD_X + HEAD_W - 25, // eye centre x
      EYE_Y = NOSE_Y, // eye centre y
      PUPIL_D = EYE_D / 2.0, // pupil diameter
      MOUTH_W = 80, // mouth width
      MOUTH_X = HEAD_X + HEAD_W - MOUTH_W, // mouth left
      MOUTH_Y = HEAD_Y + HEAD_H - CHIN_H, // mouth top
      BUN_W = 70, // width of bun
      BUN_H = 45, // height of bun
      BUN_R = 10, // round corner radius of bun
      PAT_W = 84, // patty width
      PAT_H = 7; // patty height
      NEAR = 100, // how near for the mouth to start to open
      MOUTH_H = BUN_H + 20; // maximum mouth height (gape)
      
function setup() {
   createCanvas(600, 400);
}

function draw() {
   let burgerX; // centre x of the burger
   if (mouseY > MOUTH_Y + BUN_H / 2.0 && mouseY < MOUTH_Y + MOUTH_H - BUN_H / 2.0) {
       burgerX = max(NOSE_X - MOUTH_W + PAT_W / 2.0, mouseX);
   } else {
       burgerX = max(NOSE_X + PAT_W / 2.0, mouseX);
   }
   let mouthH; // height of the mouth (gape)
   let dx = mouseX - PAT_W / 2.0 - NOSE_X; // face to patty x distance
   if (dx >= NEAR) {
      mouthH = 0;
   } else if (dx <= 0) {
      mouthH = MOUTH_H;
   } else {
      mouthH = MOUTH_H * (1.0 - dx / NEAR);
   }
   background(220);
   // nose
   stroke(0);
   fill(255);
   triangle(NOSE_X, NOSE_Y, NOSE_X, NOSE_Y + NOSE_H, NOSE_X + NOSE_W, NOSE_Y + NOSE_H);
   // hair
   stroke(0);
   fill(0);
   rect(HEAD_X, HEAD_Y, HEAD_W, HAIR_H + HEAD_R, HEAD_R);
   // chin
   stroke(0);
   fill(255);
   rect(HEAD_X, HEAD_Y + HEAD_H - CHIN_H - HEAD_R + mouthH, HEAD_W, CHIN_H + HEAD_R, HEAD_R);
   // face
   noStroke();
   fill(255);
   rect(HEAD_X, HEAD_Y + HAIR_H, HEAD_W, HEAD_H - HAIR_H - CHIN_H + mouthH);
   // eye
   stroke(0);
   fill(255);
   ellipse(EYE_X, EYE_Y, EYE_D);
   // pupil
   let theta = atan2(mouseY - EYE_Y, burgerX - EYE_X);
   fill(0);
   ellipse(EYE_X + EYE_D * cos(theta) / 4.0, EYE_Y + EYE_D * sin(theta) / 4.0, PUPIL_D);
   // head outline
   noFill();
   stroke(0);
   rect(HEAD_X, HEAD_Y, HEAD_W, HEAD_H + mouthH, HEAD_R);
   // mouth
   fill(220);
   stroke(0);
   rect(MOUTH_X, MOUTH_Y, MOUTH_W + 10, mouthH);
   noStroke();
   rect(HEAD_X + HEAD_W + 1, MOUTH_Y - 10, 20, mouthH + 20);
   // burger 
   rectMode(CENTER);
   fill(255);
   stroke(0);
   rect(burgerX, mouseY, BUN_W, BUN_H, BUN_R);
   fill(0);
   rect(burgerX, mouseY, PAT_W, PAT_H, BUN_R);
   rectMode(CORNER);
}
