// Use this Modernizr script to detect a browser's ability to smooth fonts.
// The following is adapted from...
/*
 * TypeHelpers version 1.0
 * Zoltan Hawryluk, Nov 24 2009.  
 * 
 * Released under the MIT License. http://www.opensource.org/licenses/mit-license.php
 * 
 */Modernizr.addTest("fontsmoothing",function(){if(typeof screen.fontSmoothingEnabled!="undefined")return screen.fontSmoothingEnabled;try{var a=document.createElement("canvas"),b=!1,c=!1,d=document.body||function(){c=!0;return document.documentElement.appendChild(document.createElement("body"))}();a.width="35";a.height="35";a.style.display="none";d.appendChild(a);var e=a.getContext("2d");e.textBaseline="top";e.font="32px Arial";e.fillStyle="black";e.strokeStyle="black";e.fillText("O",0,0);for(var f=8;f<=32;f++)for(var g=1;g<=32;g++){var h=e.getImageData(g,f,1,1).data,i=h[3];if(i!=255&&i!=0){b=!0;break}}d.removeChild(a);c&&document.documentElement.removeChild(d);return b}catch(j){d.removeChild(a);c&&document.documentElement.removeChild(d);return!1}});