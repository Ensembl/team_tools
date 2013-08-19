
$(document).ready(function() {

    var filterCheckUrlListUrl =
      "/cgi-bin/jh13/filter_check_url_list";

    var field_update =
      function(field) { field.td.text(field.value); };

    var countFieldList =
      [
       { key: "running",   name: "Running",   },
       { key: "failed",    name: "Failed",    },
       { key: "succeeded", name: "Succeeded", },
       { key: "total",     name: "Total",     },
       ];

    var countFieldByKey = { };
    $.each(countFieldList,
           function(i, field)
           {
             countFieldByKey[field.key] = field;
           });

    var field_delta =
      function(key, delta)
      {
        var field = countFieldByKey[key];
        field.value += delta;
        field_update(field);
      };

    var field_increment =
      function(key) { field_delta(key, +1); };
    var field_decrement =
      function(key) { field_delta(key, -1); };

    var field_result =
      function(key) { field_increment(key); field_decrement('running'); };

    (function() {
      var div = $(".filter_status");
      var table = $(document.createElement("table"));
      div.append(table);
      var tbody = $(document.createElement("tbody"));
      table.append(tbody);
      $.each(countFieldList,
             function(i, field)
             {
               field.value = 0;
               var tr = $(document.createElement("tr"));
               tbody.append(tr);
               var name_td = $(document.createElement("td"));
               tr.append(name_td);
               name_td.text(field.name);
               var value_td = $(document.createElement("td"));
               tr.append(value_td);
               field.td = value_td;
               field_update(field);
             });
    })();

    var ulByStatus = { };
    var specList =
      [
       { select: ".filter_failed",    status: 'failed'    },
       { select: ".filter_succeeded", status: 'succeeded' },
       ];
    $.each(specList, function(i, spec) {
        var div = $(spec.select);
        var ul = $(document.createElement("ul"));
        div.append(ul);
        ulByStatus[spec.status] = ul;
      });

    var ulByStatusVersion = {
      failed:    { },
      succeeded: { },
    };
    var statusCallback = function (status, version, filter, url) {
      return function() {
        var versionUl = ulByStatusVersion[status][version];
        if ( versionUl === undefined ) {
          var versionLi = $(document.createElement("li"));
          versionLi.text(version);
          ulByStatus[status].append(versionLi);
          versionUl = $(document.createElement("ul"));
          ulByStatusVersion[status][version] = versionUl;
          versionLi.append(versionUl);
        }
        var li = $(document.createElement("li"));
        versionUl.append(li);
        var a = $(document.createElement("a"));
        a.attr({ href: url, });
        a.text(filter);
        li.append(a);
        field_result(status);
      };
    };

    // add a list item element for each database

    var doVersionFilterUrl = function(version, filterUrl) {
      var filter = filterUrl.filter;
      var url = filterUrl.url;
      $.ajax({
          url: url, 
            dataType: 'text',
            error:    statusCallback('failed',     version, filter, url),
            success:  statusCallback('succeeded',  version, filter, url),
            });
      field_increment('total');
      field_increment('running');
    };

    var doVersionUrlCollection = function(versionUrlCollection) {
      var version = versionUrlCollection.version;
      var urlCollection = versionUrlCollection.url;
      $.each(urlCollection, function(i, filterUrl) {
          doVersionFilterUrl(version, filterUrl);
        });
    };

    var doUrlCollection = function(urlCollection) {
      $.each(urlCollection, function(i, versionUrlCollection) {
          doVersionUrlCollection(versionUrlCollection);
        });
    };

    $.getJSON(filterCheckUrlListUrl, {
        dataset: 'cat',
          dataset_param: 'cat_dev',
          config_path_extra: '/tmp/otter_config',
      }, doUrlCollection);
  });
