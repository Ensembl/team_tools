
$(document).ready(function() {

    var
      DBRootURL = "/cgi-bin/otter-web-utils/",
      DBListURL = DBRootURL + "db_list",
      DBCheckGeneDuplicateStableIDsURL
        = DBRootURL + "db_check_gene_duplicate_stable_ids",
      DBCheckExonDuplicateStableIDCountURL
        = DBRootURL + "db_check_exon_duplicate_stable_id_count",
      DBCheckExonDuplicateStableIDsURL
        = DBRootURL + "db_check_exon_duplicate_stable_ids",
      DBCheckTranscriptDuplicateStableIDsURL
        = DBRootURL + "db_check_transcript_duplicate_stable_ids";

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

    var addGeneDuplicateStableIDs = function(td,duplicateStableIDs) {

      var chrUL, geneStableIDUL, geneEntryTBODY;

      var addChromosome = function(duplicateStableID) {
        var chrName = duplicateStableID.chr_name;
        var chrID   = duplicateStableID.chr_id;
        var chrText = chrName + "[" + chrID +"]";
        var chrLI = $(document.createElement("li"));
        chrUL.append(chrLI);
        chrLI.text(chrText);
        geneStableIDUL = $(document.createElement("ul"));
        chrLI.append(geneStableIDUL);
      }

      var addGeneStableID = function(duplicateStableID) {
        var geneStableID = duplicateStableID.gene_sid;
        var geneStableIDLI = $(document.createElement("li"));
        geneStableIDUL.append(geneStableIDLI);
        geneStableIDLI.text(geneStableID);
        var geneEntryTABLE = $(document.createElement("table"));
        geneStableIDLI.append(geneEntryTABLE);
        geneEntryTBODY = $(document.createElement("tbody"));
        geneEntryTABLE.append(geneEntryTBODY);
      }

      var addGeneEntry = function(duplicateStableID) {
        var geneID = duplicateStableID.gene_id;
        var geneEntryTR = $(document.createElement("tr"));
        geneEntryTBODY.append(geneEntryTR);
        var geneEntryIDTD = $(document.createElement("td"));
        geneEntryTR.append(geneEntryIDTD);
        geneEntryIDTD.text(geneID);
      }

      var chrID = null, geneStableID = null, geneID = null;

      chrUL = $(document.createElement("ul"));
      td.append(chrUL);
      $.each(duplicateStableIDs,function(i,duplicateStableID) {

          // new chromosome?
          var chrIDNew = duplicateStableID.chr_id;
          if ( chrIDNew !== chrID ) {
            chrID = chrIDNew;
            geneStableID = null;
            addChromosome(duplicateStableID);
          }

          // new stable ID?
          var geneStableIDNew = duplicateStableID.gene_sid;
          if ( geneStableIDNew !== geneStableID ) {
            geneStableID = geneStableIDNew;
            geneID = null;
            addGeneStableID(duplicateStableID);
          }

          // new gene entry?
          var geneIDNew = duplicateStableID.gene_id;
          if ( geneIDNew !== geneID ) {
            geneID = geneIDNew;
            addGeneEntry(duplicateStableID);
          }

        });
    };

    var addDatabaseCheckGeneDuplicateStableIDsTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckGeneDuplicateStableIDsURL,query,function(duplicateStableIDs) {
          td.removeClass("progress_loading");
          if ( duplicateStableIDs.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addGeneDuplicateStableIDs(td,duplicateStableIDs);
          }
          else {
            td.addClass("progress_ok");
            td.text("\u263A");
          }
        });
      return td;
    };

    var addTranscriptDuplicateStableIDs = function(td,duplicateStableIDs) {

      var chrUL, transcriptStableIDUL, transcriptEntryTBODY;

      var addChromosome = function(duplicateStableID) {
        var chrName = duplicateStableID.chr_name;
        var chrID   = duplicateStableID.chr_id;
        var chrText = chrName + "[" + chrID +"]";
        var chrLI = $(document.createElement("li"));
        chrUL.append(chrLI);
        chrLI.text(chrText);
        transcriptStableIDUL = $(document.createElement("ul"));
        chrLI.append(transcriptStableIDUL);
      }

      var addTranscriptStableID = function(duplicateStableID) {
        var transcriptStableID = duplicateStableID.transcript_sid;
        var transcriptStableIDLI = $(document.createElement("li"));
        transcriptStableIDUL.append(transcriptStableIDLI);
        transcriptStableIDLI.text(transcriptStableID);
        var transcriptEntryTABLE = $(document.createElement("table"));
        transcriptStableIDLI.append(transcriptEntryTABLE);
        transcriptEntryTBODY = $(document.createElement("tbody"));
        transcriptEntryTABLE.append(transcriptEntryTBODY);
      }

      var addTranscriptEntry = function(duplicateStableID) {
        var transcriptID = duplicateStableID.transcript_id;
        var transcriptEntryTR = $(document.createElement("tr"));
        transcriptEntryTBODY.append(transcriptEntryTR);
        var transcriptEntryIDTD = $(document.createElement("td"));
        transcriptEntryTR.append(transcriptEntryIDTD);
        transcriptEntryIDTD.text(transcriptID);
      }

      var chrID = null, transcriptStableID = null, transcriptID = null;

      chrUL = $(document.createElement("ul"));
      td.append(chrUL);
      $.each(duplicateStableIDs,function(i,duplicateStableID) {

          // new chromosome?
          var chrIDNew = duplicateStableID.chr_id;
          if ( chrIDNew !== chrID ) {
            chrID = chrIDNew;
            transcriptStableID = null;
            addChromosome(duplicateStableID);
          }

          // new stable ID?
          var transcriptStableIDNew = duplicateStableID.transcript_sid;
          if ( transcriptStableIDNew !== transcriptStableID ) {
            transcriptStableID = transcriptStableIDNew;
            transcriptID = null;
            addTranscriptStableID(duplicateStableID);
          }

          // new transcript entry?
          var transcriptIDNew = duplicateStableID.transcript_id;
          if ( transcriptIDNew !== transcriptID ) {
            transcriptID = transcriptIDNew;
            addTranscriptEntry(duplicateStableID);
          }

        });
    };

    var addDatabaseCheckTranscriptDuplicateStableIDsTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckTranscriptDuplicateStableIDsURL,query,function(duplicateStableIDs) {
          td.removeClass("progress_loading");
          if ( duplicateStableIDs.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addTranscriptDuplicateStableIDs(td,duplicateStableIDs);
          }
          else {
            td.addClass("progress_ok");
            td.text("\u263A");
          }
        });
      return td;
    };

    var addDatabaseCheckExonDuplicateStableIDCountTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("???");
      $.getJSON(DBCheckExonDuplicateStableIDCountURL,query,
                function(duplicateStableIDCount) {
                  var countText = duplicateStableIDCount[0][0];
                  var count = parseInt(countText,10);
                  td.removeClass("progress_loading");
                  if ( count ) {
                    td.text(countText);
                  }
                  else {
                    td.addClass("progress_ok");
                    td.text("\u263A");
                  }
                });
      return td;
    };

    var addExonDuplicateStableIDs = function(td,duplicateStableIDs) {

      var chrUL, exonStableIDUL, exonEntryTBODY;

      var addChromosome = function(duplicateStableID) {
        var chrName = duplicateStableID.chr_name;
        var chrID   = duplicateStableID.chr_id;
        var chrText = chrName + "[" + chrID +"]";
        var chrLI = $(document.createElement("li"));
        chrUL.append(chrLI);
        chrLI.text(chrText);
        exonStableIDUL = $(document.createElement("ul"));
        chrLI.append(exonStableIDUL);
      }

      var addExonStableID = function(duplicateStableID) {
        var exonStableID = duplicateStableID.exon_sid;
        var exonStableIDLI = $(document.createElement("li"));
        exonStableIDUL.append(exonStableIDLI);
        exonStableIDLI.text(exonStableID);
        var exonEntryTABLE = $(document.createElement("table"));
        exonStableIDLI.append(exonEntryTABLE);
        exonEntryTBODY = $(document.createElement("tbody"));
        exonEntryTABLE.append(exonEntryTBODY);
      }

      var addExonEntry = function(duplicateStableID) {
        var exonID = duplicateStableID.exon_id;
        var exonEntryTR = $(document.createElement("tr"));
        exonEntryTBODY.append(exonEntryTR);
        var exonEntryIDTD = $(document.createElement("td"));
        exonEntryTR.append(exonEntryIDTD);
        exonEntryIDTD.text(exonID);
      }

      var chrID = null, exonStableID = null, exonID = null;

      chrUL = $(document.createElement("ul"));
      td.append(chrUL);
      $.each(duplicateStableIDs,function(i,duplicateStableID) {

          // new chromosome?
          var chrIDNew = duplicateStableID.chr_id;
          if ( chrIDNew !== chrID ) {
            chrID = chrIDNew;
            exonStableID = null;
            addChromosome(duplicateStableID);
          }

          // new stable ID?
          var exonStableIDNew = duplicateStableID.exon_sid;
          if ( exonStableIDNew !== exonStableID ) {
            exonStableID = exonStableIDNew;
            exonID = null;
            addExonStableID(duplicateStableID);
          }

          // new exon entry?
          var exonIDNew = duplicateStableID.exon_id;
          if ( exonIDNew !== exonID ) {
            exonID = exonIDNew;
            addExonEntry(duplicateStableID);
          }

        });
    };

    var addDatabaseCheckExonDuplicateStableIDsTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckExonDuplicateStableIDsURL,query,function(duplicateStableIDs) {
          td.removeClass("progress_loading");
          if ( duplicateStableIDs.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addExonDuplicateStableIDs(td,duplicateStableIDs);
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
            addDatabaseCheckGeneDuplicateStableIDsTD(query,tr);
            addDatabaseCheckTranscriptDuplicateStableIDsTD(query,tr);
            addDatabaseCheckExonDuplicateStableIDCountTD(query,tr);
            addDatabaseCheckExonDuplicateStableIDsTD(query,tr);
          });
      });

  });
