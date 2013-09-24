
$(document).ready(function() {

    var
      DBRootURL = "/cgi-bin/otter-web-utils/",
      DBListURL = DBRootURL + "db_list",
      DBAdjacenciesURL = DBRootURL + "assembly_adjacent";

    var addDatabaseTR = function(tbody) {
      var tr = $(document.createElement("tr"));
      tbody.append(tr);
      return tr;
    };

    var addDatabaseNameTD = function(db,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.text(db);
      return td;
    };

    var addDatabaseAdjacenciesTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBAdjacenciesURL,query,function(adjacencies) {
          td.removeClass("progress_loading"); 
          td.addClass("progress_ok");
          td.empty;
          td.text(adjacencies.count);
        });
      return td;
    };

    var tbody = $(".db_list");
    $.getJSON(DBListURL,function(dbs) {
        $.each(dbs,function(i,db) {
            var tr = addDatabaseTR(tbody);
            addDatabaseNameTD(db,tr);
            var query = { db: db, };
            addDatabaseAdjacenciesTD(query,tr);
          });
      });

  });
