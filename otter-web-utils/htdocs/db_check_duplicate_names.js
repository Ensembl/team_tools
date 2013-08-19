
$(document).ready(function() {

    var
      DBRootURL = "/cgi-bin/jh13/",
      DBListURL = DBRootURL + "db_list",
      DBCheckGeneDuplicateNamesURL       = DBRootURL + "db_check_gene_duplicate_names",
      DBCheckTranscriptDuplicateNamesURL = DBRootURL + "db_check_transcript_duplicate_names";

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

    var addGeneDuplicateNames = function(td,duplicateNames) {

      var chrUL, geneNameUL, geneEntryTBODY;

      var addChromosome = function(duplicateName) {
        var chrName = duplicateName.chr_name;
        var chrID   = duplicateName.chr_id;
        var chrText = chrName + "[" + chrID +"]";
        var chrLI = $(document.createElement("li"));
        chrUL.append(chrLI);
        chrLI.text(chrText);
        geneNameUL = $(document.createElement("ul"));
        chrLI.append(geneNameUL);
      }

      var addGeneName = function(duplicateName) {
        var geneName = duplicateName.gene_name;
        var geneNameLI = $(document.createElement("li"));
        geneNameUL.append(geneNameLI);
        geneNameLI.text(geneName);
        var geneEntryTABLE = $(document.createElement("table"));
        geneNameLI.append(geneEntryTABLE);
        geneEntryTBODY = $(document.createElement("tbody"));
        geneEntryTABLE.append(geneEntryTBODY);
      }

      var addGeneEntry = function(duplicateName) {
        var geneID = duplicateName.gene_id;
        var geneStableID = duplicateName.gene_sid;
        var geneText = geneStableID + "[" + geneID + "]";
        var start  = duplicateName.start;
        var end    = duplicateName.end;
        var strand = duplicateName.strand;
        var geneEntryTR = $(document.createElement("tr"));
        geneEntryTBODY.append(geneEntryTR);
        var geneEntryTextTD = $(document.createElement("td"));
        geneEntryTR.append(geneEntryTextTD);
        geneEntryTextTD.text(geneText);
        var geneEntryStartTD = $(document.createElement("td"));
        geneEntryTR.append(geneEntryStartTD);
        geneEntryStartTD.addClass("feature_start");
        geneEntryStartTD.text(start);
        var geneEntryEndTD = $(document.createElement("td"));
        geneEntryTR.append(geneEntryEndTD);
        geneEntryEndTD.addClass("feature_end");
        geneEntryEndTD.text(end);
        var geneEntryStrandTD = $(document.createElement("td"));
        geneEntryTR.append(geneEntryStrandTD);
        geneEntryStrandTD.addClass("feature_strand");
        geneEntryStrandTD.html(strand === "1" ? "+" : "&ndash;");
      }

      var chrID = null, geneName = null, geneID = null;

      chrUL = $(document.createElement("ul"));
      td.append(chrUL);
      $.each(duplicateNames,function(i,duplicateName) {

          // new chromosome?
          var chrIDNew = duplicateName.chr_id;
          if ( chrIDNew !== chrID ) {
            chrID = chrIDNew;
            geneName = null;
            addChromosome(duplicateName);
          }

          // new gene name?
          var geneNameNew = duplicateName.gene_name;
          if ( geneNameNew !== geneName ) {
            geneName = geneNameNew;
            geneID = null;
            addGeneName(duplicateName);
          }

          // new gene entry?
          var geneIDNew = duplicateName.gene_id;
          if ( geneIDNew !== geneID ) {
            geneID = geneIDNew;
            addGeneEntry(duplicateName);
          }

        });
    };

    var addDatabaseCheckGeneDuplicateNamesTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckGeneDuplicateNamesURL,query,function(duplicateNames) {
          td.removeClass("progress_loading");
          if ( duplicateNames.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addGeneDuplicateNames(td,duplicateNames);
          }
          else {
            td.addClass("progress_ok");
            td.text("\u263A");
          }
        });
      return td;
    };

    var addTranscriptDuplicateNames = function(td,duplicateNames) {

      var chrUL, transcriptNameUL, transcriptEntryTBODY;

      var addChromosome = function(duplicateName) {
        var chrName = duplicateName.chr_name;
        var chrID   = duplicateName.chr_id;
        var chrText = chrName + "[" + chrID +"]";
        var chrLI = $(document.createElement("li"));
        chrUL.append(chrLI);
        chrLI.text(chrText);
        transcriptNameUL = $(document.createElement("ul"));
        chrLI.append(transcriptNameUL);
      }

      var addTranscriptName = function(duplicateName) {
        var transcriptName = duplicateName.transcript_name;
        var transcriptNameLI = $(document.createElement("li"));
        transcriptNameUL.append(transcriptNameLI);
        transcriptNameLI.text(transcriptName);
        var transcriptEntryTABLE = $(document.createElement("table"));
        transcriptNameLI.append(transcriptEntryTABLE);
        transcriptEntryTBODY = $(document.createElement("tbody"));
        transcriptEntryTABLE.append(transcriptEntryTBODY);
      }

      var addTranscriptEntry = function(duplicateName) {
        var transcriptID = duplicateName.transcript_id;
        var transcriptStableID = duplicateName.transcript_sid;
        var transcriptText = transcriptStableID + "[" + transcriptID + "]";
        var start  = duplicateName.start;
        var end    = duplicateName.end;
        var strand = duplicateName.strand;
        var transcriptEntryTR = $(document.createElement("tr"));
        transcriptEntryTBODY.append(transcriptEntryTR);
        var transcriptEntryTextTD = $(document.createElement("td"));
        transcriptEntryTR.append(transcriptEntryTextTD);
        transcriptEntryTextTD.text(transcriptText);
        var transcriptEntryStartTD = $(document.createElement("td"));
        transcriptEntryTR.append(transcriptEntryStartTD);
        transcriptEntryStartTD.addClass("feature_start");
        transcriptEntryStartTD.text(start);
        var transcriptEntryEndTD = $(document.createElement("td"));
        transcriptEntryTR.append(transcriptEntryEndTD);
        transcriptEntryEndTD.addClass("feature_end");
        transcriptEntryEndTD.text(end);
        var transcriptEntryStrandTD = $(document.createElement("td"));
        transcriptEntryTR.append(transcriptEntryStrandTD);
        transcriptEntryStrandTD.addClass("feature_strand");
        transcriptEntryStrandTD.html(strand === "1" ? "+" : "&ndash;");
      }

      var chrID = null, transcriptName = null, transcriptID = null;

      chrUL = $(document.createElement("ul"));
      td.append(chrUL);
      $.each(duplicateNames,function(i,duplicateName) {

          // new chromosome?
          var chrIDNew = duplicateName.chr_id;
          if ( chrIDNew !== chrID ) {
            chrID = chrIDNew;
            transcriptName = null;
            addChromosome(duplicateName);
          }

          // new transcript name?
          var transcriptNameNew = duplicateName.transcript_name;
          if ( transcriptNameNew !== transcriptName ) {
            transcriptName = transcriptNameNew;
            transcriptID = null;
            addTranscriptName(duplicateName);
          }

          // new transcript ID?
          var transcriptIDNew = duplicateName.transcript_id;
          if ( transcriptIDNew !== transcriptID ) {
            transcriptID = transcriptIDNew;
            addTranscriptEntry(duplicateName);
          }

        });
    };

    var addDatabaseCheckTranscriptDuplicateNamesTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckTranscriptDuplicateNamesURL,query,function(duplicateNames) {
          td.removeClass("progress_loading");
          if ( duplicateNames.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addTranscriptDuplicateNames(td,duplicateNames);
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
            addDatabaseCheckGeneDuplicateNamesTD(query,tr);
            addDatabaseCheckTranscriptDuplicateNamesTD(query,tr);
          });
      });

  });
