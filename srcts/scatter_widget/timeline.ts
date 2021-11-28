import { ScatterWidget } from "./scatter_widget";
import { pauseIcon, playIcon } from "./icons";

export class Timeline {
  private parentDiv: HTMLElement;
  private scatterWidget: ScatterWidget;
  private timeline: HTMLElement;
  private scrubber: HTMLElement;
  private playPauseButton: HTMLElement;
  private timelineWidth: number;
  private scrubberWidth: number = 20;

  private mouseDown: boolean = false;
  private currentPosition: number;
  private candidatePosition: number;
  private lastMousePosition: number;

  constructor(scatterWidget: ScatterWidget) {
    this.scatterWidget = scatterWidget;
    this.parentDiv = scatterWidget.getContainerElement();

    let timeline = document.createElement("div");
    timeline.className = "scatterWidgetTimeline";

    let scrubber = document.createElement("div");
    scrubber.style.left = "0px";
    scrubber.style.width = this.scrubberWidth + "px";
    scrubber.style.height = this.scrubberWidth + "px";
    scrubber.className = "scatterWidgetScrubber";

    scrubber.onmousedown = (e) => {
      this.mouseDown = true;
      this.lastMousePosition = e.clientX;
      this.scatterWidget.setIsPaused(true);
    };

    this.parentDiv.onmousemove = (e) => {
      e.preventDefault();
      if (this.mouseDown) {
        this.currentPosition = parseInt(this.scrubber.style.left);

        this.candidatePosition = this.currentPosition +
          (e.clientX - this.lastMousePosition);
        this.candidatePosition = Math.min(
          this.timelineWidth,
          this.candidatePosition,
        );
        this.candidatePosition = Math.max(0, this.candidatePosition);
        this.scrubber.style.left = this.candidatePosition + "px";
        this.scatterWidget.setTime(
          this.candidatePosition / (this.timelineWidth + 1),
        );
        console.log(this.candidatePosition);
        this.lastMousePosition = e.clientX;
      }
    };

    // document element so mouseup can be from anywhere
    document.documentElement.onmouseup = () => {
      this.mouseDown = false;
    };

    // prevent scrubber 'sticking' to mouse if the mouse leaves the page
    document.documentElement.onmouseleave = () => {
      this.mouseDown = false;
    };

    this.playPauseButton = this.addButton(
      "playPause",
      "Play / Pause",
      pauseIcon,
      () =>
        this.scatterWidget.setIsPaused(
          !this.scatterWidget.getIsPaused(),
        ),
    );

    timeline.appendChild(scrubber);
    this.timeline = timeline;
    this.scrubber = scrubber;
  }

  public updatePosition(newPos: number) {
    if (!this.mouseDown) {
      this.scrubber.style.left = Math.floor(this.timelineWidth * newPos) + "px";
    }
  }

  public updatePlayPauseIcon(isPaused: boolean) {
    if (isPaused) {
      this.playPauseButton.innerHTML = playIcon;
    } else {
      this.playPauseButton.innerHTML = pauseIcon;
    }
  }

  private addButton(
    name: string,
    hoverText: string,
    icon: string,
    buttonCallback: Function,
  ) {
    let button = document.createElement("button");
    button.innerHTML = icon;
    button.title = hoverText;
    button.className = `${name}Button`;
    button.onclick = () => buttonCallback();
    this.parentDiv.appendChild(button);
    return button;
  }

  public getElement() {
    return this.timeline;
  }

  public resize(newHeight: number, newPos: number) {
    this.timeline.style.top = newHeight - 30 + "px";
    this.playPauseButton.style.top = newHeight - 40 + "px";
    this.timelineWidth = this.timeline.offsetWidth - this.scrubberWidth;
    this.updatePosition(newPos);
  }
}