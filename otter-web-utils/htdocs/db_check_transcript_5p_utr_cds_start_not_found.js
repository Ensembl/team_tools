
$(document).ready(function() {

    var
      DBRootURL = "/cgi-bin/jh13/",
      DBListURL = DBRootURL + "db_list",
      DBCheckExonsURL = DBRootURL + "db_check_transcript_5p_utr_cds_start_not_found";

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

      var chrUL, geneUL, transcriptUL;

      var addChromosome = function(result) {
        var chrName = result.chr_name;
        var chrID   = result.chr_id;
        var chrText = chrName + "[" + chrID +"]";
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
      }

      var chrID = null, geneID = null, transcriptID = null;

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
