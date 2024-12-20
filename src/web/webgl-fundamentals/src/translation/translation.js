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

function createProgram(gl, vertexShader, fragmentShader) {
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

function setRectangle(gl, x, y, width, height) {
    var x1 = x;
    var x2 = x + width;
    var y1 = y;
    var y2 = y + height;

    // NOTE: gl.bufferData(gl.ARRAY_BUFFER, ...) will affect whatever buffer is
    // bound to the `ARRAY_BUFFER` bind point

    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        x1, y1,
        x2, y1,
        x1, y2,
        x1, y2,
        x2, y1,
        x2, y2]), gl.STATIC_DRAW);
}

// Returns a random integer from 0 to range - 1.
function randomInt(range) {
    return Math.floor(Math.random() * range);
}

function drawScene() {
    resizeCanvasToDisplaySize(gl.canvas)
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.useProgram(program);
    gl.bindVertexArray(vao);
    gl.enableVertexAttribArray(positionAttributeLocation);
    gl.uniform2f(resolutionUniformLocation, gl.canvas.width, gl.canvas.height);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    setRectangle(gl, translation[0], translation[1], width, height);
    gl.uniform4fv(colorUniformLocation, color);
    gl.vertexAttribPointer(positionAttributeLocation, size, type, normalize, stride, offset)

    var primitiveType = gl.TRIANGLES;
    var offset = 0;
    var count = 6;
    gl.drawArrays(primitiveType, offset, count);
}

// Create both shaders
var vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);

// Create the program linking both shaders
var program = createProgram(gl, vertexShader, fragmentShader);

// Get the location of the input attribute for the vertex shader. This gets the
// input coordinates
var positionAttributeLocation = gl.getAttribLocation(program, "a_position");
var resolutionUniformLocation = gl.getUniformLocation(program, "u_resolution");
var colorUniformLocation = gl.getUniformLocation(program, "u_color");

// Create a buffer to hold the positions and bind it as ARRAY_BUFFER
var positionBuffer = gl.createBuffer();
// gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);


// Create a vertex array, bind it and enable it in the position bound to a_position
var vao = gl.createVertexArray();
// gl.bindVertexArray(vao);
// gl.enableVertexAttribArray(positionAttributeLocation);

// Set up how to pull data from the buffer into the vertex array
var size = 2;          // 2 components per iteration
var type = gl.FLOAT;   // the data is 32bit floats
var normalize = false; // don't normalize the data
var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
var offset = 0;        // start at the beginning of the buffer

// This also binds positionBuffer (the current ARRAY_BUFFER) to the a_position attribute
// gl.vertexAttribPointer(positionAttributeLocation, size, type, normalize, stride, offset)

var translation = [0, 0];
var width = 100;
var height = 30;
var color = [Math.random(), Math.random(), Math.random(), 1];

drawScene()
