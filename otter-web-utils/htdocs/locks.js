
$(document).ready(function() {

    var
      dbRootURL      = "/cgi-bin/otter-web-utils/",
      dsNamesURL     = dbRootURL + "ds_list",
      LocksURL       = dbRootURL + "locks",
      LocksDeleteURL = dbRootURL + "locks_delete";
      ;

    var contigLockFields =
      [
       { name: "contig_lock_id", title: "[id]",   },
       { name: "timestamp",      title: "Time",   },
       { name: "author_name",    title: "Author", },
       { name: "author_id",      title: "[id]",   },
       { name: "hostname",       title: "Host",   },
       { name: "contig_name",    title: "Contig", },
       { name: "contig_id",      title: "[id]",   },
       ];

    var contigFields =
      [
       { name: "clone_name", title: "Clones",      },
       { name: "chr_name",   title: "Chromosomes", },
       ];

    var allFields = [ ].concat(contigLockFields,contigFields);

    // a comparison function to order lists of locks by timestamp
    var locksCompareByTimestamp = function(ls0,ls1) {
      var t0 = ls0[0].timestamp;
      var t1 = ls1[0].timestamp;
      if      (t0 < t1 ) { return -1; }
      else if (t0 > t1 ) { return +1; }
      else               { return  0; }
    };

    var locksGroupByContigLockId = function(locks) {
      var locksByContigLockId = [ ];
      $.each(locks,
             function(i,lock) {
               var contigLockId = lock.contig_lock_id;
               var locksForThisContigLockId = null;
               $.each(locksByContigLockId,
                      function(i,locksForOneContigLockId) {
                        if ( locksForOneContigLockId[0].contig_lock_id === contigLockId ) {
                          locksForThisContigLockId = locksForOneContigLockId;
                          return false;
                        }
                      });
               if (locksForThisContigLockId === null) {
                 locksForThisContigLockId = [ ];
                 locksByContigLockId.push(locksForThisContigLockId);
               }
               locksForThisContigLockId.push(lock);
             });
      return locksByContigLockId;
    };

    var addHeaderTrElement = function(tbody) {
      var tr = $(document.createElement("tr"));
      tbody.append(tr);
      tr.append($(document.createElement("th"))); // checkboxes
      $.each(allFields,
             function(i,field) {
               var th = $(document.createElement("th"));
               tr.append(th);
               th.text(field.title);
             });
    };

    var addContigLockTdElements = function(lock,tr,rowCount) {
      var td = $(document.createElement("td"));
      td.attr({ rowspan: rowCount, });
      tr.append(td);
      var checkbox = $(document.createElement("input"));
      checkbox.attr({ type: "checkbox", });
      td.append(checkbox);
      $.each(contigLockFields,
             function(i,field) {
               var td = $(document.createElement("td"));
               td.attr({ rowspan: rowCount, });
               tr.append(td);
               td.text(lock[field.name]);
             });
      return {
        checkbox: checkbox,
      };
    };

    var addContigTdElements = function(lock,tr) {
      $.each(contigFields,
             function(i,field) {
               var td = $(document.createElement("td"));
               tr.append(td);
               td.text(lock[field.name]);
             });
    };

    var addLockTrElements = function(locks,tbody) {
      var rowCount = locks.length;
      var contigLockTdElements;
      var isFirstLock = true;
      $.each(locks,
             function(i,lock) {
               var tr = $(document.createElement("tr"));
               tbody.append(tr);
               if ( isFirstLock ) {
                 contigLockTdElements = addContigLockTdElements(lock,tr,rowCount);
               }
               addContigTdElements(lock,tr);
               isFirstLock = false;
             });
      return {
        checkbox: contigLockTdElements.checkbox,
      };
    };

    var addLocksTableElement = function(locks,div) {
      var locksByContigLockId = locksGroupByContigLockId(locks);
      locksByContigLockId.sort(locksCompareByTimestamp);
      var table = $(document.createElement("table"));
      div.append(table);
      var tbody = $(document.createElement("tbody"));
      tbody.addClass("lock_list");
      table.append(tbody);
      addHeaderTrElement(tbody);
      var lockEntries = [ ];
      $.each(locksByContigLockId,
             function(i,locks) {
               var lockTrElements = addLockTrElements(locks,tbody);
               var lockEntry = {
                 contig_lock_id: locks[0].contig_lock_id,
                 checkbox:       lockTrElements.checkbox,
               };
               lockEntries.push(lockEntry);
             });
      return {
        lockEntries: lockEntries,
      };
    };

    var dsLoadLocks = function(ds) {
      var div = ds.div;
      div.addClass("progress_loading");
      div.text("LOADING");
      var query = { ds: ds.name, };
      $.getJSON(LocksURL,query,function(locks) {
          div.removeClass("progress_loading");
          div.text("");
          if (locks.length) {
            var locksTableElement = addLocksTableElement(locks,div);
            ds.lockEntries = locksTableElement.lockEntries;
          } else {
            ds.lockEntries = [ ];
          }
        });
    };

    var dbLocksDelete = function(ds) {
      var locks = [ ];
      $.each(ds.lockEntries,
             function(i,lockEntry) {
               if (lockEntry.checkbox.attr("checked")) {
                 locks.push(lockEntry.contig_lock_id);
               }
             });
      var query = {
          ds:    ds.name,
          locks: JSON.stringify(locks),
      };
      $.getJSON(LocksDeleteURL,query,function(result) {
          alert(
                "succeeded: " + JSON.stringify(result.succeeded) + "\n" +
                "failed:    " + JSON.stringify(result.failed)    + "\n" +
                "bogus:     " + JSON.stringify(result.bogus)     + "\n" +
                "");
          dsLoadLocks(ds);
        });
    }

    // create a "Reload" button for a database
    var dbReloadButtonNew = function(db) {
      var button = $(document.createElement("input"));
      var attrs = {
        type:    "submit",
        value:   "Reload",
      };
      button.attr(attrs);
      button.click(function() { dsLoadLocks(db); });
      return button;
    };

    // create a "Delete Selected Locks" button for a database
    var dbDeleteSelectedLocksButtonNew = function(db) {
      var button = $(document.createElement("input"));
      var attrs = {
        type:    "submit",
        value:   "Delete Selected Locks",
      };
      button.attr(attrs);
      button.click(function() { dbLocksDelete(db); });
      return button;
    };

    // create a list item element for a database
    var dbLiElementNew = function(db) {
      db.div = $(document.createElement("div"));
      var li = $(document.createElement("li"));
      li.text(db.name);
      li.append($(document.createElement("br")));
      li.append(dbReloadButtonNew(db));
      li.append(dbDeleteSelectedLocksButtonNew(db));
      li.append(db.div);
      dsLoadLocks(db);
      return li;
    };

    // add a list item element for each database
    $.getJSON(dsNamesURL,function(dsNames) {
        var ul = $(".db_list");
        $.each(dsNames,
               function(i,dsName) {
                 var ds = {
                   name: dsName,
                 };
                 ul.append(dbLiElementNew(ds));
               });
      });

  });
