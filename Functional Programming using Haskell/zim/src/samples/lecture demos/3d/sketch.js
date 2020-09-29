

function preload() {
}

function setup() {
    createCanvas(800, 500, WEBGL);
}

const RADIUS = 200;
let angle = 0;

function draw() {
    // draw
        background(200, 255, 200);
        ambientLight(50);
        pointLight(200, 200, 200, 0, 0, 0);
        rotateX(PI);
        
        specularMaterial(250);
        sphere(100);
        rotateY(angle);
        translate(RADIUS, 0, 0);
        rotateY(angle * 2);
        sphere(30);
        rotateY(angle * 2);
        translate(70, 0, 0);
        sphere(5);
    
    // animate
       angle += PI / 120;
}