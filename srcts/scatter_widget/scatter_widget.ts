import * as THREE from 'three';
import { ProjectionMatrix, ScatterInputData, Config, Matrix } from './data';
import { multiply, centerColumns, getColMeans } from './utils'
import { FRAGMENT_SHADER, VERTEX_SHADER } from './shaders';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import { playIcon, pauseIcon, resetIcon } from './icons'
import './style.css'

export class ScatterWidget {
    private container: HTMLElement;
    private canvas: HTMLCanvasElement = document.createElement("canvas");
    private scene: THREE.Scene;
    private camera: THREE.PerspectiveCamera;
    private renderer: THREE.WebGLRenderer;
    private config: Config;
    private dataset: Matrix;
    private projectionMatrices: Array<ProjectionMatrix>;
    private clock = new THREE.Clock();
    private time: number;
    private oldFrame: number;
    private pointsBuffers: THREE.BufferAttribute[] = [];
    private points: THREE.Points;
    private axisSegments: THREE.LineSegments;
    private minPointSize: number = 0.02;
    private orbitControls: OrbitControls;
    private isPaused: boolean;
    private colMeans: Matrix;
    private mapping: { colour: string[] };
    private pointColours: THREE.BufferAttribute;

    constructor(containerElement: HTMLElement, width: number, height: number) {

        let scene = new THREE.Scene()
        scene.background = new THREE.Color(0xfffffff);
        const light = new THREE.AmbientLight(0x404040); // soft white light
        scene.add(light);
        this.scene = scene;

        this.canvas.width = width;
        this.canvas.height = height;
        this.canvas.id = `${containerElement.id}-canvas`
        this.canvas.className = "scatterWidgetCanvas"

        this.container = containerElement;
        this.container.appendChild(this.canvas)
        this.container.className = "scatterWidgetContainer"

        this.renderer = new THREE.WebGLRenderer({
            canvas: this.canvas
        });

        this.camera = new THREE.PerspectiveCamera(45, width / height, 0.01, 1000);
        this.camera.position.setZ(4);

        this.renderer.setPixelRatio(window.devicePixelRatio);
        this.renderer.setSize(width, height);
        this.renderer.render(this.scene, this.camera);

        this.addControls();
    }

    private constructPlot() {

        //todo: implement this check in the R code
        if (this.projectionMatrices[0][0].length != 3) {
            throw new TypeError(`Projection matrix must be of dimension 3. got ${this.projectionMatrices[0][0].length}`)
        }
        this.colMeans = getColMeans(this.dataset);

        this.pointColours = this.coloursToBufferAttribute(this.mapping.colour)

        let pointsGeometry = new THREE.BufferGeometry();
        let pointSize: number = this.dataset.length ** (-1 / 3)

        let pointsMaterial = new THREE.ShaderMaterial({
            uniforms: {
                size: { value: Math.max(pointSize, this.minPointSize) }
            },
            vertexShader: VERTEX_SHADER,
            fragmentShader: FRAGMENT_SHADER,
        });

        let pointsBuffer = this.getPointsBuffer(0, this.config.center)
        pointsGeometry.setAttribute('position', pointsBuffer);

        if (this.config.cacheFrames) {
            this.pointsBuffers.push(pointsBuffer)
        }

        this.points = new THREE.Points(pointsGeometry, pointsMaterial)
        this.points.geometry.setAttribute('color', this.pointColours)
        this.scene.add(this.points)

        let axisLinesGeometry = new THREE.BufferGeometry()
        let axisLinesMaterial = new THREE.LineBasicMaterial({ color: 0x000000, linewidth: 1 })

        let axisLinesBuffer = this.getAxisLinesBuffer(0)
        axisLinesGeometry.setAttribute('position', axisLinesBuffer)

        this.axisSegments = new THREE.LineSegments(axisLinesGeometry, axisLinesMaterial)
        this.scene.add(this.axisSegments)

        this.orbitControls = new OrbitControls(this.camera, this.renderer.domElement);

        this.clock = new THREE.Clock();
        this.time = 0;
        this.oldFrame = -1;

        this.isPaused = false;
    }

    public resize(newWidth: number, newHeight: number) {
        this.canvas.width = newWidth;
        this.canvas.height = newHeight;
        this.camera.aspect = newWidth / newHeight;
        this.camera.updateProjectionMatrix()
        this.renderer.setSize(newWidth, newHeight)
    }

    public renderValue(inputData: ScatterInputData) {
        this.config = inputData.config;
        this.dataset = inputData.dataset;
        this.projectionMatrices = inputData.projectionMatrices;
        this.mapping = inputData.mapping

        this.constructPlot();
        this.animate();
    }

    private getPointsBuffer(i: number, center: boolean): THREE.BufferAttribute {
        let positionMatrix: Matrix = multiply(this.dataset, this.projectionMatrices[i]);

        if (center) {
            let colMeans = multiply(this.colMeans, this.projectionMatrices[i]);
            positionMatrix = centerColumns(positionMatrix, colMeans)
        }

        let flattenedPositionMatrix = new Float32Array([].concat(...positionMatrix));
        return new THREE.BufferAttribute(flattenedPositionMatrix, 3)
    }

    private getAxisLinesBuffer(i: number): THREE.BufferAttribute {
        let projectionMatrix = this.projectionMatrices[i]
        let linesBufferMatrix = projectionMatrix.map(row => [0, 0, 0].concat(row))
        return new THREE.BufferAttribute(new Float32Array([].concat(...linesBufferMatrix)), 3)
    }

    private coloursToBufferAttribute(colours: string[]): THREE.BufferAttribute {
        let colour = new THREE.Color;
        let bufferArray = new Float32Array(this.dataset.length * 3)

        if (colours.length > 0) {
            let j = 0;
            for (let i = 0; i < colours.length; i++) {
                j = 3 * i;
                colour.set(colours[i])
                bufferArray[j] = colour.r
                bufferArray[j + 1] = colour.g
                bufferArray[j + 2] = colour.b
            }
        }
        return new THREE.BufferAttribute(bufferArray, 3)
    }

    private addControls() {

        let playPauseButton = document.createElement("button");
        playPauseButton.innerHTML = pauseIcon;
        playPauseButton.className = "playPauseButton";
        playPauseButton.onclick = () => this.setIsPaused(!this.getIsPaused())
        this.container.appendChild(playPauseButton);

        let resetButton = document.createElement("button");
        resetButton.innerHTML = resetIcon;
        resetButton.className = "resetButton";
        resetButton.onclick = () => this.resetClock();
        this.container.appendChild(resetButton);
    }

    private animate() {
        let delta = this.clock.getDelta();

        if (!this.getIsPaused()) {
            this.time += delta;
        }

        if (this.time >= this.config.duration) this.time = 0;

        let currentFrame = Math.floor(this.time * this.config.fps);

        if (currentFrame != this.oldFrame) {
            let frameBuffer: THREE.BufferAttribute

            if (this.pointsBuffers[currentFrame] == undefined) {
                frameBuffer = this.getPointsBuffer(currentFrame, this.config.center)
                if (this.config.cacheFrames) {
                    this.pointsBuffers[currentFrame] = frameBuffer
                }
            }
            else {
                frameBuffer = this.pointsBuffers[currentFrame]
            }

            this.points.geometry.setAttribute('position', frameBuffer);
            this.points.geometry.attributes.position.needsUpdate = true;

            this.axisSegments.geometry.setAttribute('position', this.getAxisLinesBuffer(currentFrame))
            this.axisSegments.geometry.attributes.position.needsUpdate = true;

            this.oldFrame = currentFrame;
        }

        this.renderer.render(this.scene, this.camera);

        requestAnimationFrame(() => this.animate());
    }

    private getIsPaused(): boolean {
        return this.isPaused
    }

    private setIsPaused(isPaused: boolean) {
        this.isPaused = isPaused
        let playPauseButton = this.container.querySelector('.playPauseButton')

        if (!isPaused) {
            this.animate()
            playPauseButton.innerHTML = pauseIcon
        }
        else {
            playPauseButton.innerHTML = playIcon
        }
    }

    private resetClock() {
        this.time = 0
    }
}