let img;

function preload() {
   img = loadImage("images/b.jpg");
}

function setup() {
    createCanvas(img.width, img.height);
}

function draw() {
    image(img, 0, 0);
}