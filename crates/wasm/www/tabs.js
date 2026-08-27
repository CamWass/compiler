export class TabGroup {
  #tabs;

  constructor(args) {
    this.#tabs = args.tabs;

    for (const tab of args.tabs) {
      const button = document.createElement("button");
      button.textContent = tab.label;
      button.addEventListener("click", () => {
        for (const otherTab of args.tabs) {
          if (otherTab !== tab) {
            otherTab.contentContainer.style.display = "none";
          }
        }
        tab.contentContainer.style.display = "initial";
      });
      args.buttonContainer.appendChild(button);
    }
  }

  updateSuccess(result) {
    for (const tab of this.#tabs) {
      tab.updateSuccess(result);
    }
  }

  updateError(error) {
    for (const tab of this.#tabs) {
      tab.updateError(error);
    }
  }
}
