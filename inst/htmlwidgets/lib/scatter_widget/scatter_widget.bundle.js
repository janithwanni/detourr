var scatter_widget;(()=>{"use strict";var t,e={976:(t,e,n)=>{n.d(e,{Z:()=>o});var r=n(645),i=n.n(r)()((function(t){return t[1]}));i.push([t.id,"button {\n  background-color: transparent;\n  position: absolute;\n  color: rgb(0, 0, 0);\n  text-align: center;\n  width: 30px;\n  height: 30px;\n  border-color: transparent;\n}\n\nbutton:hover {\n  color: rgb(56, 56, 56);\n}\n\nbutton.playPauseButton {\n  top: 10px;\n  left: 10px;\n}\n\nbutton.resetButton {\n  top: 45px;\n  left: 10px;\n}\n\n.scatterWidgetContainer {\n  position: relative;\n}\n\n.scatterWidgetCanvas {\n  position: absolute;\n}\n",""]);const o=i},645:t=>{t.exports=function(t){var e=[];return e.toString=function(){return this.map((function(e){var n=t(e);return e[2]?"@media ".concat(e[2]," {").concat(n,"}"):n})).join("")},e.i=function(t,n,r){"string"==typeof t&&(t=[[null,t,""]]);var i={};if(r)for(var o=0;o<this.length;o++){var a=this[o][0];null!=a&&(i[a]=!0)}for(var s=0;s<t.length;s++){var c=[].concat(t[s]);r&&i[c[0]]||(n&&(c[2]?c[2]="".concat(n," and ").concat(c[2]):c[2]=n),e.push(c))}},e}},252:(t,e,n)=>{n.r(e),n.d(e,{default:()=>g});var r=n(379),i=n.n(r),o=n(795),a=n.n(o),s=n(569),c=n.n(s),u=n(565),l=n.n(u),f=n(216),d=n.n(f),h=n(589),p=n.n(h),v=n(976),m={};m.styleTagTransform=p(),m.setAttributes=l(),m.insert=c().bind(null,"head"),m.domAPI=a(),m.insertStyleElement=d(),i()(v.Z,m);const g=v.Z&&v.Z.locals?v.Z.locals:void 0},379:t=>{var e=[];function n(t){for(var n=-1,r=0;r<e.length;r++)if(e[r].identifier===t){n=r;break}return n}function r(t,r){for(var o={},a=[],s=0;s<t.length;s++){var c=t[s],u=r.base?c[0]+r.base:c[0],l=o[u]||0,f="".concat(u," ").concat(l);o[u]=l+1;var d=n(f),h={css:c[1],media:c[2],sourceMap:c[3]};-1!==d?(e[d].references++,e[d].updater(h)):e.push({identifier:f,updater:i(h,r),references:1}),a.push(f)}return a}function i(t,e){var n=e.domAPI(e);return n.update(t),function(e){if(e){if(e.css===t.css&&e.media===t.media&&e.sourceMap===t.sourceMap)return;n.update(t=e)}else n.remove()}}t.exports=function(t,i){var o=r(t=t||[],i=i||{});return function(t){t=t||[];for(var a=0;a<o.length;a++){var s=n(o[a]);e[s].references--}for(var c=r(t,i),u=0;u<o.length;u++){var l=n(o[u]);0===e[l].references&&(e[l].updater(),e.splice(l,1))}o=c}}},569:t=>{var e={};t.exports=function(t,n){var r=function(t){if(void 0===e[t]){var n=document.querySelector(t);if(window.HTMLIFrameElement&&n instanceof window.HTMLIFrameElement)try{n=n.contentDocument.head}catch(t){n=null}e[t]=n}return e[t]}(t);if(!r)throw new Error("Couldn't find a style target. This probably means that the value for the 'insert' parameter is invalid.");r.appendChild(n)}},216:t=>{t.exports=function(t){var e=document.createElement("style");return t.setAttributes(e,t.attributes),t.insert(e),e}},565:(t,e,n)=>{t.exports=function(t){var e=n.nc;e&&t.setAttribute("nonce",e)}},795:t=>{t.exports=function(t){var e=t.insertStyleElement(t);return{update:function(n){!function(t,e,n){var r=n.css,i=n.media,o=n.sourceMap;i?t.setAttribute("media",i):t.removeAttribute("media"),o&&"undefined"!=typeof btoa&&(r+="\n/*# sourceMappingURL=data:application/json;base64,".concat(btoa(unescape(encodeURIComponent(JSON.stringify(o))))," */")),e.styleTagTransform(r,t)}(e,t,n)},remove:function(){!function(t){if(null===t.parentNode)return!1;t.parentNode.removeChild(t)}(e)}}}},589:t=>{t.exports=function(t,e){if(e.styleSheet)e.styleSheet.cssText=t;else{for(;e.firstChild;)e.removeChild(e.firstChild);e.appendChild(document.createTextNode(t))}}},28:(t,e)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.resetIcon=e.playIcon=e.pauseIcon=void 0,e.pauseIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="pause" class="svg-inline--fa fa-pause fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M144 479H48c-26.5 0-48-21.5-48-48V79c0-26.5 21.5-48 48-48h96c26.5 0 48 21.5 48 48v352c0 26.5-21.5 48-48 48zm304-48V79c0-26.5-21.5-48-48-48h-96c-26.5 0-48 21.5-48 48v352c0 26.5 21.5 48 48 48h96c26.5 0 48-21.5 48-48z"></path></svg>\n',e.playIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="play" class="svg-inline--fa fa-play fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M424.4 214.7L72.4 6.6C43.8-10.3 0 6.1 0 47.9V464c0 37.5 40.7 60.1 72.4 41.3l352-208c31.4-18.5 31.5-64.1 0-82.6z"></path></svg>\n',e.resetIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="redo-alt" class="svg-inline--fa fa-redo-alt fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M256.455 8c66.269.119 126.437 26.233 170.859 68.685l35.715-35.715C478.149 25.851 504 36.559 504 57.941V192c0 13.255-10.745 24-24 24H345.941c-21.382 0-32.09-25.851-16.971-40.971l41.75-41.75c-30.864-28.899-70.801-44.907-113.23-45.273-92.398-.798-170.283 73.977-169.484 169.442C88.764 348.009 162.184 424 256 424c41.127 0 79.997-14.678 110.629-41.556 4.743-4.161 11.906-3.908 16.368.553l39.662 39.662c4.872 4.872 4.631 12.815-.482 17.433C378.202 479.813 319.926 504 256 504 119.034 504 8.001 392.967 8 256.002 7.999 119.193 119.646 7.755 256.455 8z"></path></svg>'},467:(t,e,n)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.ScatterWidget=void 0;var r=n(96);Object.defineProperty(e,"ScatterWidget",{enumerable:!0,get:function(){return r.ScatterWidget}})},96:function(t,e,n){var r=this&&this.__createBinding||(Object.create?function(t,e,n,r){void 0===r&&(r=n),Object.defineProperty(t,r,{enumerable:!0,get:function(){return e[n]}})}:function(t,e,n,r){void 0===r&&(r=n),t[r]=e[n]}),i=this&&this.__setModuleDefault||(Object.create?function(t,e){Object.defineProperty(t,"default",{enumerable:!0,value:e})}:function(t,e){t.default=e}),o=this&&this.__importStar||function(t){if(t&&t.__esModule)return t;var e={};if(null!=t)for(var n in t)"default"!==n&&Object.prototype.hasOwnProperty.call(t,n)&&r(e,t,n);return i(e,t),e};Object.defineProperty(e,"__esModule",{value:!0}),e.ScatterWidget=void 0;var a=o(n(212)),s=n(681),c=n(148),u=n(886),l=n(28);n(252);var f=function(){function t(t,e,n){this.canvas=document.createElement("canvas"),this.clock=new a.Clock,this.pointsBuffers=[],this.minPointSize=.02;var r=new a.Scene;r.background=new a.Color(268435455);var i=new a.AmbientLight(4210752);r.add(i),this.scene=r,this.canvas.width=e,this.canvas.height=n,this.canvas.id=t.id+"-canvas",this.canvas.className="scatterWidgetCanvas",this.container=t,this.container.appendChild(this.canvas),this.container.className="scatterWidgetContainer",this.renderer=new a.WebGLRenderer({canvas:this.canvas}),this.camera=new a.PerspectiveCamera(45,e/n,.01,1e3),this.camera.position.setZ(4),this.renderer.setPixelRatio(window.devicePixelRatio),this.renderer.setSize(e,n),this.renderer.render(this.scene,this.camera),this.addControls()}return t.prototype.constructPlot=function(){if(3!=this.projectionMatrices[0][0].length)throw new TypeError("Projection matrix must be of dimension 3. got "+this.projectionMatrices[0][0].length);this.colMeans=s.getColMeans(this.dataset),this.pointColours=this.coloursToBufferAttribute(this.mapping.colour);var t=new a.BufferGeometry,e=Math.pow(this.dataset.length,-1/3),n=new a.ShaderMaterial({uniforms:{size:{value:Math.max(e,this.minPointSize)}},vertexShader:c.VERTEX_SHADER,fragmentShader:c.FRAGMENT_SHADER}),r=this.getPointsBuffer(0,this.config.center);t.setAttribute("position",r),this.config.cacheFrames&&this.pointsBuffers.push(r),this.points=new a.Points(t,n),this.points.geometry.setAttribute("color",this.pointColours),this.scene.add(this.points);var i=new a.BufferGeometry,o=new a.LineBasicMaterial({color:0,linewidth:1}),l=this.getAxisLinesBuffer(0);i.setAttribute("position",l),this.axisSegments=new a.LineSegments(i,o),this.scene.add(this.axisSegments),this.orbitControls=new u.OrbitControls(this.camera,this.renderer.domElement),this.clock=new a.Clock,this.time=0,this.oldFrame=-1,this.isPaused=!1},t.prototype.resize=function(t,e){this.canvas.width=t,this.canvas.height=e,this.camera.aspect=t/e,this.camera.updateProjectionMatrix(),this.renderer.setSize(t,e)},t.prototype.renderValue=function(t){this.config=t.config,this.dataset=t.dataset,this.projectionMatrices=t.projectionMatrices,this.mapping=t.mapping,this.constructPlot(),this.animate()},t.prototype.getPointsBuffer=function(t,e){var n=s.multiply(this.dataset,this.projectionMatrices[t]);if(e){var r=s.multiply(this.colMeans,this.projectionMatrices[t]);n=s.centerColumns(n,r)}var i=new Float32Array([].concat.apply([],n));return new a.BufferAttribute(i,3)},t.prototype.getAxisLinesBuffer=function(t){var e=this.projectionMatrices[t].map((function(t){return[0,0,0].concat(t)}));return new a.BufferAttribute(new Float32Array([].concat.apply([],e)),3)},t.prototype.coloursToBufferAttribute=function(t){var e=new a.Color,n=new Float32Array(3*this.dataset.length);if(t.length>0)for(var r=0,i=0;i<t.length;i++)r=3*i,e.set(t[i]),n[r]=e.r,n[r+1]=e.g,n[r+2]=e.b;return new a.BufferAttribute(n,3)},t.prototype.addControls=function(){var t=this,e=document.createElement("button");e.innerHTML=l.pauseIcon,e.className="playPauseButton",e.onclick=function(){return t.setIsPaused(!t.getIsPaused())},this.container.appendChild(e);var n=document.createElement("button");n.innerHTML=l.resetIcon,n.className="resetButton",n.onclick=function(){return t.resetClock()},this.container.appendChild(n)},t.prototype.animate=function(){var t=this,e=this.clock.getDelta();this.getIsPaused()||(this.time+=e),this.time>=this.config.duration&&(this.time=0);var n=Math.floor(this.time*this.config.fps);if(n!=this.oldFrame){var r=void 0;null==this.pointsBuffers[n]?(r=this.getPointsBuffer(n,this.config.center),this.config.cacheFrames&&(this.pointsBuffers[n]=r)):r=this.pointsBuffers[n],this.points.geometry.setAttribute("position",r),this.points.geometry.attributes.position.needsUpdate=!0,this.axisSegments.geometry.setAttribute("position",this.getAxisLinesBuffer(n)),this.axisSegments.geometry.attributes.position.needsUpdate=!0,this.oldFrame=n}this.renderer.render(this.scene,this.camera),requestAnimationFrame((function(){return t.animate()}))},t.prototype.getIsPaused=function(){return this.isPaused},t.prototype.setIsPaused=function(t){this.isPaused=t;var e=this.container.querySelector(".playPauseButton");t?e.innerHTML=l.playIcon:(this.animate(),e.innerHTML=l.pauseIcon)},t.prototype.resetClock=function(){this.time=0},t}();e.ScatterWidget=f},148:(t,e)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.VERTEX_SHADER=e.FRAGMENT_SHADER=void 0,e.FRAGMENT_SHADER="\nvarying float vSize;\nvarying vec3 vColor;\n\nvoid main(){\n    gl_FragColor = vec4( vColor, 1.0 );\n\n    // make points circular\n    float distance = length(2.0 * gl_PointCoord - 1.0);\n    if (distance > 1.0) {\n        discard;\n    }\n}\n",e.VERTEX_SHADER="\nuniform float size;\nattribute vec3 color;\n\nvarying vec3 vColor;\n\nvoid main(){\n    vColor=color;\n    vec4 mvPosition = modelViewMatrix * vec4( position, 1.0);\n    gl_Position = projectionMatrix * mvPosition;\n    gl_PointSize = 200.0 * size / -mvPosition.z;\n}\n"},681:(t,e)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.centerColumns=e.getColMeans=e.multiply=void 0,e.multiply=function(t,e){for(var n=t.length,r=t[0].length,i=new Array(n),o=0;o<n;++o){var a=new Array(3);i[o]=a;for(var s=t[o],c=0;c<3;++c){for(var u=0,l=0;l<r;++l)u+=s[l]*e[l][c];a[c]=u}}return i},e.getColMeans=function(t){var e=t.length;return[t.reduce((function(t,e){return t.map((function(t,n){return t+e[n]}))})).map((function(t){return t/e}))]},e.centerColumns=function(t,e){return t.map((function(t){return t.map((function(t,n){return t-e[0][n]}))}))}}},n={};function r(t){var i=n[t];if(void 0!==i)return i.exports;var o=n[t]={id:t,exports:{}};return e[t].call(o.exports,o,o.exports,r),o.exports}r.m=e,t=[],r.O=(e,n,i,o)=>{if(!n){var a=1/0;for(u=0;u<t.length;u++){for(var[n,i,o]=t[u],s=!0,c=0;c<n.length;c++)(!1&o||a>=o)&&Object.keys(r.O).every((t=>r.O[t](n[c])))?n.splice(c--,1):(s=!1,o<a&&(a=o));s&&(t.splice(u--,1),e=i())}return e}o=o||0;for(var u=t.length;u>0&&t[u-1][2]>o;u--)t[u]=t[u-1];t[u]=[n,i,o]},r.n=t=>{var e=t&&t.__esModule?()=>t.default:()=>t;return r.d(e,{a:e}),e},r.d=(t,e)=>{for(var n in e)r.o(e,n)&&!r.o(t,n)&&Object.defineProperty(t,n,{enumerable:!0,get:e[n]})},r.o=(t,e)=>Object.prototype.hasOwnProperty.call(t,e),r.r=t=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},(()=>{var t={499:0};r.O.j=e=>0===t[e];var e=(e,n)=>{var i,o,[a,s,c]=n,u=0;for(i in s)r.o(s,i)&&(r.m[i]=s[i]);if(c)var l=c(r);for(e&&e(n);u<a.length;u++)o=a[u],r.o(t,o)&&t[o]&&t[o][0](),t[a[u]]=0;return r.O(l)},n=self.webpackChunkscatter_widget=self.webpackChunkscatter_widget||[];n.forEach(e.bind(null,0)),n.push=e.bind(null,n.push.bind(n))})();var i=r.O(void 0,[841],(()=>r(467)));i=r.O(i),scatter_widget=i})();