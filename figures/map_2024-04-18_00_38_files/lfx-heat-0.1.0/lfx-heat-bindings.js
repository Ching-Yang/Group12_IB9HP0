!function(e){var t={};function a(r){if(t[r])return t[r].exports;var o=t[r]={i:r,l:!1,exports:{}};return e[r].call(o.exports,o,o.exports,a),o.l=!0,o.exports}a.m=e,a.c=t,a.d=function(e,t,r){a.o(e,t)||Object.defineProperty(e,t,{configurable:!1,enumerable:!0,get:r})},a.r=function(e){Object.defineProperty(e,"__esModule",{value:!0})},a.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return a.d(t,"a",t),t},a.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},a.p="",a(a.s=0)}([function(e,t){function a(e,t){var a=null;return e&&("string"==typeof t?a=e.properties[t]:"function"==typeof t&&(a=t(e))),a}function r(e,t,r,o,n,i){var s=function e(t,r){var o=[];if(void 0===t||null===t)return o;if("string"==typeof t&&(t=JSON.parse(t)),"Topology"===t.type){var n=[];for(var i in t.objects){var s=topojson.feature(t,t.objects[i]);L.Util.isArray(s)?n=n.concat(s):"features"in s?n=n.concat(s.features):n.push(s)}return e(n,r)}var u=L.Util.isArray(t)?t:t.features;return u?$.each(u,function(t,n){var i=null,s=null;"Point"===n.geometry.type?(i=parseFloat(n.geometry.coordinates[1]),s=parseFloat(n.geometry.coordinates[0]),i&&s&&(r?o.push([i,s,a(n,r)]):o.push([i,s]))):"MultiPoint"===n.geometry.type&&(o=o.concat(e(n,r)))}):"Feature"===t.type&&$.each(t.geometry.coordinates,function(e,n){var i,s;i=parseFloat(n[1]),s=parseFloat(n[0]),i&&s&&(r?o.push([i,s,a(t,r)]):o.push([i,s]))}),o}(t,r);if(!$.isEmptyObject(s)){var u=L.heatLayer(s,i);e.layerManager.addLayer(u,"heatmap",o,n)}}LeafletWidget.methods.addHeatmap=function(e,t,a,r){if(!$.isEmptyObject(e)){var o=L.heatLayer(e,r);this.layerManager.addLayer(o,"heatmap",t,a)}},LeafletWidget.methods.addGeoJSONHeatmap=function(e,t,a,o,n){var i=this;LeafletWidget.utils.isURL(e)?$.getJSON(e,function(e){r(i,e,t,a,o,n)}):r(i,e,t,a,o,n)},LeafletWidget.methods.addKMLHeatmap=function(e,t,a,o,n){var i=this;if(LeafletWidget.utils.isURL(e))$.getJSON(e,function(e){var s=toGeoJSON.kml(LeafletWidget.utils.parseXML(e));r(i,s,t,a,o,n)});else{var s=toGeoJSON.kml(LeafletWidget.utils.parseXML(e));r(i,s,t,a,o,n)}},LeafletWidget.methods.addCSVHeatmap=function(e,t,a,o,n,i){var s=this;LeafletWidget.utils.isURL(e)?$.getJSON(e,function(e){csv2geojson.csv2geojson(e,i||{},function(e,i){r(s,i,t,a,o,n)})}):csv2geojson.csv2geojson(e,i||{},function(e,i){r(s,i,t,a,o,n)})},LeafletWidget.methods.addGPXHeatmap=function(e,t,a,o,n){var i=this;if(LeafletWidget.utils.isURL(e))$.getJSON(e,function(e){var s=toGeoJSON.gpx(LeafletWidget.utils.parseXML(e));r(i,s,t,a,o,n)});else{var s=toGeoJSON.gpx(LeafletWidget.utils.parseXML(e));r(i,s,t,a,o,n)}},LeafletWidget.methods.removeHeatmap=function(e){this.layerManager.removeLayer("heatmap",e)},LeafletWidget.methods.clearHeatmap=function(){this.layerManager.clearLayers("heatmap")}}]);
//# sourceMappingURL=lfx-heat-bindings.js.map