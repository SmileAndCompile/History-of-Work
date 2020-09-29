// YinYang (static)

const W = 804, H = 454;

function setup() {
   createCanvas(W, H);
   background(200);
   noLoop();
}

const XC = W / 2; // centre = (XC, YC)
const YC = H / 2;
const R = 150;   // radius of outer circle

function draw() {
   noStroke();
   fill(0);
   ellipse(XC, YC, R * 2, R * 2, HALF_PI, PI + HALF_PI);
   fill(256);
   arc(XC, YC, R * 2, R * 2, PI + HALF_PI, HALF_PI);
   ellipse(XC, YC - R / 2.0, R, R);
   fill(0);
   ellipse(XC, YC + R / 2.0, R, R);
   ellipse(XC, YC - R / 2.0, R / 3.0, R / 3.0)
   fill(256);
   ellipse(XC, YC + R / 2.0, R / 3.0, R / 3.0)
}
