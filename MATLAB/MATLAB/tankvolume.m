function v = tankvolume(h, r, H)
% tankvolume
%
% The function tankvolume calculates the volume of liquid in a tank with rounded/spherical ends.
%
% inputs: h: height of the liquid.
%         H: height of the tank.
%         r: radius of the ends.
%
% outputs: v: volume of liquid in the tank.
%
% Author: Zane Keeley
% Date: 11/10/14

if h <= r
    sphereH = h;
elseif h > r && h <= (H - r)
    sphereH = r;
elseif h > (H - r)
    sphereH = h - (H - (2*r));
end

sphereV = ((pi*(sphereH^2))/3)*(3*r - sphereH);
disp(sphereH);
disp(sphereV);

if h <= r
    cylinderH = 0;
elseif h > r && h <= (H - r)
    cylinderH = h - r;
elseif h > (H - r)
    cylinderH = H - (2*r);
end

cylinderV = pi*(r^2)*cylinderH;
disp(cylinderH);
disp(cylinderV);

v = sphereV + cylinderV;
end

