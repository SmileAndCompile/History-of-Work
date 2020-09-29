function setup() {
    createCanvas(400, 400);
}

function draw() {
    background('pink');
    // foliage
    fill('green')
    triangle(200, 40, 200 - 146 / 2, 290, 200 + 146 / 2, 290);
    // trunk
    fill('brown');
    rect(200 - 50 / 2, 290, 50, 75);

}