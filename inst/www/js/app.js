/*
 * ******************************************************************************
 *                                     app.js
 *  js
 *  Copyright © INRA 2019
 *  Creation date:  06 March, 2019
 *  Contact: arnaud.charleroy@inra.fr
 * ******************************************************************************
 */

$(function() {
  // Comment for production case
  // ocpu.seturl("http://opensilex.org:8004/ocpu/apps/OpenSILEX/compareVariablesDemo/R");

  // show graph button
  $("#submit").click(function(e) {
    e.preventDefault();
    functionsParameters = getInputs();
    // basical
    // create a plot from htmlwidget named function name .e.g plotVar with Widget.html
    showPlot("plotDiv", "eventVSEnvironmental", functionsParameters);
  });
});


function getInputs() {

  // input parameters in the form of the R function
  var scientificObject = $("#scientificObject").val();
  var endDate = $("#endDate").val();
  var startDate = $("#startDate").val();
  var showPoint = $("#showPoint").prop("checked");

  functionsParameters = {
    scientificObject: scientificObject,
    endDate: endDate,
    startDate: startDate,
    showPoint: showPoint,
  };
  return functionsParameters;
}
