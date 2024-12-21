"use strict";

const canvas = document.getElementById("canvas");
const gl = canvas.getContext("webgl2")

if (!gl) {
    throw new Error("No gl for you")
}

var vertexShaderSource = `#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 a_position;

uniform vec2 u_resolution;

// all shaders have a main function
void main() {
  vec2 clipSpace = a_position / u_resolution * 2.0 - 1.0;

  // Special variable to connect to the fragment shader
  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
}
`;

var fragmentShaderSource = `#version 300 es

// fragment shaders don't have a default precision so we need
// to pick one. highp is a good default. It means "high precision"
precision highp float;

// input colour
uniform vec4 u_color;

// we need to declare an output for the fragment shader
out vec4 outColor;

void main() {
  outColor = u_color;
}
`;

function createShader(gl, type, source) {
  var shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  var success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);

  if (success) {
    return shader;
  }

  console.log(gl.getShaderInfoLog(shader));
  gl.deleteShader(shader);
}

function createProgram(gl, vertexShaderSource, fragmentShaderSource) {
    var vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
    var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);

    var program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    var success = gl.getProgramParameter(program, gl.LINK_STATUS);
    if (success) {
        return program;
    }

    console.log(gl.getProgramInfoLog(program));
    gl.deleteProgram(program);
}

function resizeCanvasToDisplaySize(canvas) {
  // Lookup the size the browser is displaying the canvas in CSS pixels.
  const displayWidth  = canvas.clientWidth;
  const displayHeight = canvas.clientHeight;

  // Check if the canvas is not the same size.
  const needResize = canvas.width  !== displayWidth ||
                     canvas.height !== displayHeight;

  if (needResize) {
    // Make the canvas the same size
    canvas.width  = displayWidth;
    canvas.height = displayHeight;
  }

  return needResize;
}

function createRectangleCoordinateArray(x, y, w, h) {
    var x1 = x;
    var x2 = x + w;
    var y1 = y;
    var y2 = y + h;

    return new Float32Array([
        x1, y1,
        x2, y1,
        x1, y2,
        x1, y2,
        x2, y1,
        x2, y2,
    ]);
}

// Returns a random integer from 0 to range - 1.
function randomInt(range) {
    return Math.floor(Math.random() * range);
}

function resizeAndClear(gl) {
    resizeCanvasToDisplaySize(gl.canvas)
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
}

function setColorUniform(gl, program, color) {
    var colorUniformLocation = gl.getUniformLocation(program, "u_color");
    gl.uniform4fv(colorUniformLocation, color);
}

function setResolutionUniform(gl, program, w, h) {
    var resolutionUniformLocation = gl.getUniformLocation(program, "u_resolution");
    gl.uniform2f(resolutionUniformLocation, w, h);
}

// Load vertices in the buffer and connect it to a_position with the appropriate
// iteration size
function setPositionAttribute(gl, program, vertices, iterationSize) {
    var positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

    var vao = gl.createVertexArray();
    gl.bindVertexArray(vao);

    var positionAttributeLocation = gl.getAttribLocation(program, "a_position");
    gl.enableVertexAttribArray(positionAttributeLocation);

    gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW);

    var type = gl.FLOAT;   // the data is 32bit floats
    var normalize = false; // don't normalize the data
    var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
    var offset = 0;        // start at the beginning of the buffer

    // This also binds positionBuffer (the current ARRAY_BUFFER) to the a_position attribute
    gl.vertexAttribPointer(positionAttributeLocation, iterationSize, type, normalize, stride, offset);
}

function drawScene(gl, nVertices, iterationSize) {
    var primitiveType = gl.TRIANGLES;
    var offset = 0;
    var count = nVertices / iterationSize;
    gl.drawArrays(primitiveType, offset, count);
}

function drawGl(translation) {
    resizeAndClear(gl)

    var program = createProgram(gl, vertexShaderSource, fragmentShaderSource);
    gl.useProgram(program);

    // Set uniforms
    var color = [Math.random(), Math.random(), Math.random(), 1];
    setColorUniform(gl, program, color);
    setResolutionUniform(gl, program, gl.canvas.width, gl.canvas.height)

    // Set attributes
    var width = 100;
    var height = 30;
    var vertices = createRectangleCoordinateArray(translation[0], translation[1], width, height);
    var iterationSize = 2;
    setPositionAttribute(gl, program, vertices, iterationSize)

    // Draw the scene
    drawScene(gl, vertices.length, iterationSize);
}

function redraw(translation) {
    drawGl(translation);
}

function getSlider(id) {
    return document.getElementById(id);
}

function getTranslationSliders() {
    var sliderX = getSlider("x-value");
    var sliderY = getSlider("y-value");
    return {
        "x" : sliderX,
        "y" : sliderY,
    };
}

function setUI() {
    var translationSliders = getTranslationSliders()
    translationSliders.x.oninput = () => updateTranslation(translationSliders);
    translationSliders.y.oninput = () => updateTranslation(translationSliders);
}

function getTranslation(sliders) {
    return [sliders.x.value, sliders.y.value];
}

function updateTranslation(sliders) {
    redraw(getTranslation(sliders));
    
}

function main() {
    setUI();
    drawGl(getTranslation(getTranslationSliders()));
}

main()
