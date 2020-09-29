let snowman; // image

let snowX, snowY; // center of the snowman

function preload() {
   snowman = loadImage('images/snowman.png');
}

function setup() {
    createCanvas(400, 400);
    snowX = 100;
    snowY = 100;
}

function draw() {
    background(230);
    imageMode(CENTER);
    image(snowman, snowX, snowY);
}

function keyPressed() 
    print('code =' + keyCode)
    if (keyCode == RIGHT_ARROW) {
        snowX += 5;
    }
}
