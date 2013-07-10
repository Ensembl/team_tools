
$(document).ready(function() {

    var
      DBRootURL = "/cgi-bin/jh13/",
      DBListURL = DBRootURL + "db_list",
      DBCheckExonsURL = DBRootURL + "db_check_start_exon_5p_utr_phase";

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

    var addResults = function(td,results) {

        var chrUL, geneUL, transcriptUL, translationUL, exonUL;

      var addChromosome = function(result) {
        var chrName = result.chr_name;
        var chrID   = result.chr_id;
        var chrHidden = result.chr_hidden;
          var chrText = chrName + "[" + chrID + "]" + ((chrHidden == 1) ? " {hidden}" : "");
        var chrLI = $(document.createElement("li"));
        chrUL.append(chrLI);
        chrLI.text(chrText);
        geneUL = $(document.createElement("ul"));
        chrLI.append(geneUL);
      }

      var addGene = function(result) {
        var geneID = result.gene_id;
        var geneStableID = result.gene_sid;
        var geneStart = result.gene_start;
        var geneEnd = result.gene_end;
        var geneText =
        geneStableID +
        "[" + geneID + "]" +
        " " +
        "(" + geneStart + " - " + geneEnd + ")";
        var geneLI = $(document.createElement("li"));
        geneUL.append(geneLI);
        geneLI.text(geneText);
        transcriptUL = $(document.createElement("ul"));
        geneLI.append(transcriptUL);
      }

      var addTranscript = function(result) {
        var transcriptID = result.transcript_id;
        var transcriptStableID = result.transcript_sid;
        var transcriptBiotype = result.transcript_biotype;
        var transcriptStart = result.transcript_start;
        var transcriptEnd = result.transcript_end;
        var transcriptText =
        transcriptStableID +
        "[" + transcriptID + "]" +
        " " + transcriptBiotype
        + " " +
        "(" + transcriptStart + " - " + transcriptEnd + ")";
        var transcriptLI = $(document.createElement("li"));
        transcriptUL.append(transcriptLI);
        transcriptLI.text(transcriptText);
        translationUL = $(document.createElement("ul"));
        transcriptLI.append(translationUL);
      }

      var addTranslation = function(result) {
        var translationID = result.translation_id;
        var translationStableID = result.translation_sid;
        var translationStart = result.translation_start;
        var translationEnd = result.translation_end;
        var translationText =
        translationStableID +
        "[" + translationID + "]" +
        " " +
        "(" + translationStart + " - " + translationEnd + ")";
        var translationLI = $(document.createElement("li"));
        translationUL.append(translationLI);
        translationLI.text(translationText);
        exonUL = $(document.createElement("ul"));
        translationLI.append(exonUL);
      }

      var addExon = function(result) {
        var exonID = result.start_exon_id;
        var exonStableID = result.start_exon_sid;
        var exonPhase    = result.start_exon_phase;
        var exonRank     = result.start_exon_rank;
        var exonText =
        exonStableID +
        "[" + exonID + "]" +
        " phase " + exonPhase +
        ", rank " + exonRank;
        var exonLI = $(document.createElement("li"));
        exonUL.append(exonLI);
        exonLI.text(exonText);
        exonUL = $(document.createElement("ul"));
        exonLI.append(exonUL);
      }
        var chrID = null, geneID = null, transcriptID = null, translationID = null, exonID = null;

      chrUL = $(document.createElement("ul"));
      td.append(chrUL);
      $.each(results,function(i,result) {

          // new chromosome?
          var chrIDNew = result.chr_id;
          if ( chrIDNew !== chrID ) {
            chrID = chrIDNew;
            geneID = null;
            addChromosome(result);
          }

          // new gene?
          var geneIDNew = result.gene_id;
          if ( geneIDNew !== geneID ) {
            geneID = geneIDNew;
            transcriptID = null;
            addGene(result);
          }

          // new transcript?
          var transcriptIDNew = result.transcript_id;
          if ( transcriptIDNew !== transcriptID ) {
            transcriptID = transcriptIDNew;
            addTranscript(result);
          }

          // new translation?
          var translationIDNew = result.translation_id;
          if ( translationIDNew !== translationID ) {
            translationID = translationIDNew;
            addTranslation(result);
            exonID = null;
          }

          var exonIDNew = result.start_exon_id;
          if ( exonIDNew != exonID ) {
            exonID = exonIDNew;
            addExon(result);
          }
        });
    };

    var addResultsTD = function(query,tr) {
      var td = $(document.createElement("td"));
      tr.append(td);
      td.addClass("progress_loading");
      td.text("LOADING");
      $.getJSON(DBCheckExonsURL,query,function(results) {
          td.removeClass("progress_loading");
          if ( results.length ) {
            td.addClass("progress_bogus");
            td.empty();
            addResults(td,results);
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
            addResultsTD(query,tr);
          });
      });

  });
