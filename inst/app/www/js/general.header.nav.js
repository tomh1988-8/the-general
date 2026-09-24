// general.header.nav.js

(function (global, $) {
  "use strict";

  var General = (global.General = global.General || {});
  General.nav = General.nav || {};

  var chipMap = {
    frontPage: "ghn_frontPage",
    Frequencies: "ghn_Frequencies",
    Proportions: "ghn_Proportions",
    Proportions2: "ghn_Proportions",
    Averages: "ghn_Averages",
    Averages2: "ghn_Averages",
    Lines: "ghn_Lines",
    Lines2: "ghn_Lines",
    Bars: "ghn_Bars",
    Bars2: "ghn_Bars",
    Bars3: "ghn_Bars",
    Areas: "ghn_Areas",
    Areas2: "ghn_Areas",
    ScatterGrouped: "ghn_Scatter",
    Density: "ghn_Density",
    myDashboard: "ghn_Dashboard"
  };

  General.nav.setActiveChip = function (activeTabName) {
    $(".general-header-chip").removeClass("active-chip");
    $(".general-header-dropdown-item").removeClass("active");

    var chipId = chipMap[activeTabName];
    if (chipId) {
      $("#" + chipId).addClass("active-chip");
    }

    $('.general-header-dropdown-item[data-tab="' + activeTabName + '"]').addClass("active");
  };

  General.nav.goToTab = function (tabName) {
    if (!tabName || !global.Shiny || !global.Shiny.setInputValue) {
      return;
    }

    global.Shiny.setInputValue("header_nav_tab", tabName, { priority: "event" });
  };

  $(document).on("click", ".general-header-nav-item", function (e) {
    var tabName = $(this).data("tab");
    if (!tabName) {
      return;
    }

    e.preventDefault();
    General.nav.goToTab(tabName);
  });

  $(document).on("click", ".general-header-dropdown-item", function (e) {
    var tabName = $(this).data("tab");
    if (!tabName) {
      return;
    }

    e.preventDefault();
    General.nav.goToTab(tabName);
  });

  $(document).on("shiny:inputchanged", function (e) {
    if (e.name === "tabs") {
      General.nav.setActiveChip(e.value);
    }
  });

  $(function () {
    window.setTimeout(function () {
      // Added [data-value] to strictly target the actual tab anchor, bypassing parent wrapper containers
      var initialTab =
        $('aside.main-sidebar .nav-sidebar a.nav-link.active[data-value]').attr("data-value") ||
        "frontPage";

      General.nav.setActiveChip(initialTab);
    }, 150);
  });
})(window, window.jQuery);
