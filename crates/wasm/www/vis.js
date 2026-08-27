import { instance } from "./node_modules/@viz-js/viz/dist/viz.js";

const viz = await instance();

export class CFGDisplay {
  #container;
  #svg;

  constructor(container) {
    const svgContainer = document.createElement("div");
    svgContainer.classList = "svg-container";
    container.appendChild(svgContainer);

    this.#container = svgContainer;

    let viewBox = { x: 0, y: 0, w: 800, h: 600 };
    let isPanning = false;
    let startPoint = { x: 0, y: 0 };

    const updateViewBox = () => {
      this.#svg?.setAttribute(
        "viewBox",
        `${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`,
      );
    };

    // Handle Zooming on the Container
    this.#container.addEventListener("wheel", (e) => {
      e.preventDefault();
      const zoomFactor = e.deltaY < 0 ? 0.9 : 1.1;
      const rect = this.#container.getBoundingClientRect();

      const mouseX = e.clientX - rect.left;
      const mouseY = e.clientY - rect.top;

      const svgMouseX = viewBox.x + (mouseX / rect.width) * viewBox.w;
      const svgMouseY = viewBox.y + (mouseY / rect.height) * viewBox.h;

      viewBox.w *= zoomFactor;
      viewBox.h *= zoomFactor;
      viewBox.x = svgMouseX - (mouseX / rect.width) * viewBox.w;
      viewBox.y = svgMouseY - (mouseY / rect.height) * viewBox.h;

      updateViewBox();
    });

    this.#container.addEventListener("mousedown", (e) => {
      isPanning = true;
      startPoint = { x: e.clientX, y: e.clientY };
      this.#container.style.cursor = "grabbing";
    });

    window.addEventListener("mousemove", (e) => {
      if (!isPanning) return;

      const rect = this.#container.getBoundingClientRect();
      const dx = (e.clientX - startPoint.x) * (viewBox.w / rect.width);
      const dy = (e.clientY - startPoint.y) * (viewBox.h / rect.height);

      viewBox.x -= dx;
      viewBox.y -= dy;
      startPoint = { x: e.clientX, y: e.clientY };

      updateViewBox();
    });

    window.addEventListener("mouseup", () => {
      isPanning = false;
      this.#container.style.cursor = "grab";
    });
  }

  updateSuccess(cfgText) {
    if (this.#container.firstElementChild) {
      this.#container.removeChild(this.#container.firstElementChild);
    }

    if (cfgText) {
      const cfg = viz.renderSVGElement(cfgText);

      this.#svg = cfg;
      this.#container.appendChild(cfg);
    } else {
      this.#container.textContent =
        "CFG not rendered when there's more than 200 nodes";
    }
  }

  updateError() {
    if (this.#container.firstElementChild) {
      this.#container.removeChild(this.#container.firstElementChild);
    }

    this.#container.textContent = "Error";
  }
}
