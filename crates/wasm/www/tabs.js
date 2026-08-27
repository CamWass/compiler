export class TabGroup {
  #tabs;

  constructor(args) {
    this.#tabs = args.tabs;

    for (let i = 0; i < args.tabs.length; i++) {
      const tab = args.tabs[i];

      const button = document.createElement("button");
      button.textContent = tab.label;

      if (i == 0) {
        button.classList.add("active");
      }

      button.addEventListener("click", () => {
        button.classList.add("active");
        for (const otherTab of args.tabs) {
          if (otherTab !== tab) {
            otherTab.button.classList.remove("active");
            otherTab.contentContainer.style.display = "none";
          }
        }
        tab.contentContainer.style.display = "initial";
      });

      args.buttonContainer.appendChild(button);
      tab.button = button;
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
