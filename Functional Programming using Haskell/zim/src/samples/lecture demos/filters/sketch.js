const NO_FILTER = 0,
      BLUR_FILTER = 1,
      TINT_FILTER = 2;


let img;
let blurBtn, tintBtn;
let currentFilter = NO_FILTER;

function preload() {
   img = loadImage("images/b.jpg");
}

function setup() {
    createCanvas(img.width, img.height);
    blurBtn = createButton("blur");
    blurBtn.position(0, img.height + 10);
    blurBtn.mouseClicked(blurBtnClicked)
    tintBtn = createButton("tint");
    tintBtn.position(40, img.height + 10);
    tintBtn.mouseClicked(tintBtnClicked)
}

function draw() {
    image(img, 0, 0);
    switch (currentFilter) {
        case BLUR_FILTER:
            filter(BLUR, 10);
            break;
        case TINT_FILTER:
            tint(200, 255, 255, 255);
            break;
        default:
           filter(BLUR, 0);
           tint(255, 255, 255, 255);
    }
}

function blurBtnClicked() {
    print("BLUR");
    if (currentFilter == BLUR_FILTER) {
        currentFilter = NO_FILTER;
    } else {
        currentFilter = BLUR_FILTER;        
    }   
}

function tintBtnClicked() {
    print("TINT");
    if (currentFilter == TINT_FILTER) {
        currentFilter = NO_FILTER;
    } else {
        currentFilter = TINT_FILTER;        
    }
}