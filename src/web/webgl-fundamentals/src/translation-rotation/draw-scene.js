"use strict";

function getVertexShaderSource() {
    return `#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 a_position;

uniform vec2 u_resolution;
uniform vec2 u_translation;
uniform vec2 u_rotation;

// all shaders have a main function
void main() {
  vec2 rotated = vec2(
    a_position.x * u_rotation.y + a_position.y * u_rotation.x,
    a_position.y * u_rotation.y - a_position.x * u_rotation.x);

  vec2 clipSpace = (rotated + u_translation) / u_resolution * 2.0 - 1.0;

  // Special variable to connect to the fragment shader
  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
}
`;
}

function getFragmentShaderSource() {
    return `#version 300 es

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
}

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

function createProgram(gl) {
    var vertexShader = createShader(gl, gl.VERTEX_SHADER, getVertexShaderSource());
    var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, getFragmentShaderSource());

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

function createFCoordinateArray(w, h, t) {
    const x = 0;
    const y = 0;

    return new Float32Array([
        // left column
        x, y,
        x + t, y,
        x, y + h,
        x, y + h,
        x + t, y,
        x + t, y + h,
        
        // top rung
        x + t, y,
        x + w, y,
        x + t, y + t,
        x + t, y + t,
        x + w, y,
        x + w, y + t,
        
        // middle rung
        x + t, y + t * 2,
        x + w * 2 / 3, y + t * 2,
        x + t, y + t * 3,
        x + t, y + t * 3,
        x + w * 2 / 3, y + t * 2,
        x + w * 2 / 3, y + t * 3]);
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
    var location = gl.getUniformLocation(program, "u_color");
    gl.uniform4fv(location, color);
}

function setResolutionUniform(gl, program, w, h) {
    var location = gl.getUniformLocation(program, "u_resolution");
    gl.uniform2f(location, w, h);
}

function setTranslationUniform(gl, program, x, y) {
    var location = gl.getUniformLocation(program, "u_translation");
    gl.uniform2f(location, x, y);
}

function setRotationUniform(gl, program, x, y) {
    var location = gl.getUniformLocation(program, "u_rotation");
    gl.uniform2f(location, x, y);
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

function drawGl(gl, graphicsState) {
    var program = graphicsState.program;
    var iterationSize = graphicsState.iterationSize;
    var vertices = graphicsState.vertices;

    resizeAndClear(gl)
    gl.useProgram(program);

    // Set the vertex attribute here, as we won't update it (at least not for now)
    setPositionAttribute(gl, program, vertices, iterationSize)

    redrawGl(gl, graphicsState)
}

function redrawGl(gl, graphicsState) {
    // Set uniforms
    var color = graphicsState.color;
    var program = graphicsState.program;
    var translation = graphicsState.translation;
    var vertices = graphicsState.vertices;
    var iterationSize = graphicsState.iterationSize;
    var rotation = graphicsState.rotation;

    setColorUniform(gl, program, color);
    setResolutionUniform(gl, program, gl.canvas.width, gl.canvas.height);
    setTranslationUniform(gl, program, translation[0], translation[1]);
    setRotationUniform(gl, program, rotation[0], rotation[1]);

    // Draw the scene
    drawScene(gl, vertices.length, iterationSize);
}

function getSlider(id) {
    return document.getElementById(id);
}

function getUiSliders() {
    var sliderX = getSlider("x-value");
    var sliderY = getSlider("y-value");
    var sliderRotation = getSlider("rotation-value");
    return {
        "x" : sliderX,
        "y" : sliderY,
        "rotation" : sliderRotation,
    };
}

function setUI(gl, graphicsState) {
    var sliders = getUiSliders()
    sliders.x.oninput = () => updateSlider(gl, graphicsState, sliders);
    sliders.y.oninput = () => updateSlider(gl, graphicsState, sliders);
    sliders.rotation.oninput = () => updateSlider(gl, graphicsState, sliders);
}

function getTranslation(sliders) {
    return [sliders.x.valueAsNumber, sliders.y.valueAsNumber];
}

function getRotation(sliders) {
    var radians = sliders.rotation.valueAsNumber * Math.PI;
    return [Math.cos(radians), Math.sin(radians)];
}

function updateSlider(gl, graphicsState, sliders) {
    graphicsState.translation = getTranslation(sliders);
    graphicsState.rotation = getRotation(sliders);
    redrawGl(gl, graphicsState);
}

function main() {
    const canvas = document.getElementById("canvas");
    const gl = canvas.getContext("webgl2")

    if (!gl) {
        throw new Error("No gl for you")
    }

    var graphicsState = {
        color : [Math.random(), Math.random(), Math.random(), 1],
        translation : [0, 0],
        rotation : [0, 1],
        program : createProgram(gl),
        vertices : createFCoordinateArray(100, 150, 30),
        iterationSize : 2, // 2D, for now
    };

    setUI(gl, graphicsState);
    drawGl(gl, graphicsState);
}

main();
