
const N = 6; // # faces
let faceLeft = new Array(N);
let faceTop = new Array(N);
const FACE_HEIGHT = 60;
const FACE_WIDTH = 60;
const EYE_Y = 30;
const EYE_GAP = 20;
const EYE_DIAM = 15;

function makeFaces() {
    for (let i = 0; i < N; i++) {
       faceLeft[i] = random(50);
       faceTop[i] = random(350);
    }
}

function setup() {
    createCanvas(400, 400);
    makeFaces();
}

function draw() { 
   if (frameCount % 200 == 0) {
      makeFaces();
   }
   background(230);
   for (let i = 0; i < N; i++) {
      rect(faceLeft[i], faceTop[i], FACE_WIDTH, FACE_HEIGHT);
      ellipse(faceLeft[i] + FACE_WIDTH / 2.0 - EYE_GAP / 2.0, faceTop[i] + EYE_Y, EYE_DIAM);
      ellipse(faceLeft[i] + FACE_WIDTH / 2.0 + EYE_GAP / 2.0, faceTop[i] + EYE_Y, EYE_DIAM);
      faceLeft[i] += 1;
   }
}

