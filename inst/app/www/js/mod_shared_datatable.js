(function(window, $) {
  "use strict";

  function tableNamespace(tableNode) {
    var tableId = tableNode.attr("id") || Math.random().toString(36).slice(2);

    return ".generalDtAdjust" + tableId.replace(/[^A-Za-z0-9_]/g, "_");
  }

  function getFullNames(table) {
    var settings = table.settings()[0];

    return settings.aoColumns.map(function(column, index) {
      var title = column.sName || column.title || "";

      if (title) {
        return title;
      }

      return $(table.column(index).header()).text().trim();
    });
  }

  function applyHeaderEllipsis(table, fullNames) {
    table.columns().every(function(index) {
      var header = $(this.header());
      var title = fullNames[index] || header.text().trim();

      header.attr("title", title);
      header.addClass("general-dt-header-cell");

      header
        .find(".dt-column-title, .DataTables_sort_wrapper")
        .addClass("general-dt-header-label")
        .attr("title", title);
    });
  }

  function adjustTableLayout(table, fullNames) {
    applyHeaderEllipsis(table, fullNames);
    table.columns.adjust();
  }

  function scheduleAdjustments(table, fullNames) {
    [0, 50, 150, 300, 600, 900].forEach(function(delay) {
      window.setTimeout(function() {
        adjustTableLayout(table, fullNames);
      }, delay);
    });
  }

  function bindLayoutEvents(table, fullNames) {
    var tableNode = $(table.table().node());
    var eventNamespace = tableNamespace(tableNode);
    var container = $(table.table().container());
    var card = container.closest(".card, .box");

    table.off("draw.dt" + eventNamespace);
    table.on("draw.dt" + eventNamespace, function() {
      applyHeaderEllipsis(table, fullNames);
    });

    $(window).off("resize" + eventNamespace);
    $(window).on("resize" + eventNamespace, function() {
      scheduleAdjustments(table, fullNames);
    });

    if (!card.length) {
      return;
    }

    card.off(eventNamespace);
    card.on(
      "maximized.lte.cardwidget" + eventNamespace + " " +
        "minimized.lte.cardwidget" + eventNamespace + " " +
        "expanded.lte.cardwidget" + eventNamespace + " " +
        "collapsed.lte.cardwidget" + eventNamespace + " " +
        "transitionend" + eventNamespace + " " +
        "webkitTransitionEnd" + eventNamespace,
      function() {
        scheduleAdjustments(table, fullNames);
      }
    );

    card
      .find('[data-card-widget="maximize"], [data-card-widget="fullscreen"]')
      .off("click" + eventNamespace)
      .on("click" + eventNamespace, function() {
        scheduleAdjustments(table, fullNames);
      });
  }

  function initTable(settings) {
    var tableNode = $(settings.nTable);

    if (!tableNode.hasClass("general-shared-datatable")) {
      return;
    }

    var table = new $.fn.dataTable.Api(settings);
    var fullNames = getFullNames(table);

    applyHeaderEllipsis(table, fullNames);
    scheduleAdjustments(table, fullNames);
    bindLayoutEvents(table, fullNames);
  }

  $(document).on("init.dt", function(event, settings) {
    initTable(settings);
  });

  $(function() {
    $("table.general-shared-datatable").each(function() {
      if ($.fn.DataTable.isDataTable(this)) {
        initTable($(this).DataTable().settings()[0]);
      }
    });
  });
})(window, window.jQuery);