document.addEventListener("DOMContentLoaded", function () {
  const sheet = document.getElementById("bottom_sheet");
  const handle = document.getElementById("bottom_sheet_handle");
  const close = document.getElementById("bottom_sheet_close");

  if (!sheet || !handle) return;

  function openSheet() {
    sheet.classList.remove("collapsed");
    handle.setAttribute("aria-label", "Ebeneneinstellungen schließen");
    notifySheetState(true);
    updateMapForSheet(true);
  }

  function closeSheet() {
    sheet.classList.add("collapsed");
    handle.setAttribute("aria-label", "Ebeneneinstellungen öffnen");
    notifySheetState(false);
    updateMapForSheet(false);
  }

  function toggleSheet() {
    if (sheet.classList.contains("collapsed")) {
      openSheet();
    } else {
      closeSheet();
    }
  }

  handle.addEventListener("click", toggleSheet);
  if (close) close.addEventListener("click", closeSheet);

  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") closeSheet();
  });

  function notifySheetState(isOpen) {
    if (window.Shiny) {
      Shiny.setInputValue("bottom_sheet_open", isOpen, { priority: "event" });
    }
    if (!isOpen) {
      window.setTimeout(function () {
        window.dispatchEvent(new Event("resize"));
      }, 260);
    }
  }

  function updateMapForSheet(isOpen) {
    const map = window.nrwLeafletMap;
    const state = window.nrwMapState;
    if (!map || !state) return;

    if (isOpen) {
      state.openedBySheet = true;
      state.userMovedAfterOpen = false;
      map.setMaxBounds(state.relaxedBounds);
      map.options.maxBoundsViscosity = state.relaxedViscosity;
      window.setTimeout(function () {
        if (map.invalidateSize) {
          map.invalidateSize({ pan: false });
        }

        window.setTimeout(function () {
          const sheetHeight =
            sheet.getBoundingClientRect().height || window.innerHeight * 0.48;
          map.stop();
          map.panBy([0, sheetHeight * 0.58], { animate: true, duration: 0.25 });
        }, 40);
      }, 240);
    } else {
      map.setMaxBounds(state.normalBounds);
      map.options.maxBoundsViscosity = state.normalViscosity;

      if (!state.userMovedAfterOpen && state.nrwBounds) {
        map.fitBounds(state.nrwBounds, { padding: [16, 16] });
      }
      state.openedBySheet = false;
      state.userMovedAfterOpen = false;
    }
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("layer-status", function (message) {
      const status = document.getElementById("layer_status");
      if (status) status.textContent = message || "";
    });
  }
});
