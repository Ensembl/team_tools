
$(document).ready(function() {

    var
      DBRootURL = "/cgi-bin/otter-web-utils/",
      DBListURL = DBRootURL + "db_list",
      DBCheckShortContigsURL = DBRootURL + "db_check_short_contigs";

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

    var fieldList =
      [
       { "key": "asm_name",   "name": "Chromosome"  },
       { "key": "asm_length", "name": "length"      },
       { "key": "cmp_name",   "name": "Contig"      },
       { "key": "cmp_length", "name": "length"      }
       ];

    var addContigs = function (td,contigs) {
      var table = $(document.createElement("table"));
      td.append(table);
      var tbody = $(document.createElement("tbody"));
      table.append(tbody);
      var headTr = $(document.createElement("tr"));
      tbody.append(headTr);
      $.each(fieldList,function(i,field) {
          var th = $(document.createElement("th"));
          headTr.append(th);
          th.text(field.name);
        });
      $.each(contigs,function(i,contig) {
          var tr = $(document.createElement("tr"));
          tbody.append(tr);
          $.each(fieldList,function(j,field) {
              var td = $(document.createElement("td"));
              tr.append(td);
              td.text(contig[field.key]);
              if (field.name === "length") {
                td.addClass("feature_length");
              }
            });
        });
    };

    var addDatabaseCheckShortContigsTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckShortContigsURL,query,function(contigs) {
          td.removeClass("progress_loading");
          if ( contigs.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addContigs(td,contigs);
          }
          else {
            td.addClass("progress_ok");
            td.text("\u263A");
          }
        });
      return td;
    };

    var tbody = $(".db_list");
    $.getJSON(DBListURL,function(dbs) {
            $.each(dbs,function(i,db) {
            var tr = addDatabaseTR(tbody);
            addDatabaseNameTD(db,tr);
            var query = { db: db, };
            addDatabaseCheckShortContigsTD(query,tr);
          });
      });

  });
