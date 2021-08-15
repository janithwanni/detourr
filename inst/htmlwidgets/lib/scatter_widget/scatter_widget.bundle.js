var scatter_widget;(()=>{"use strict";var t,e={976:(t,e,n)=>{n.d(e,{Z:()=>o});var r=n(645),i=n.n(r)()((function(t){return t[1]}));i.push([t.id,"button {\n  background-color: transparent;\n  position: absolute;\n  color: rgb(0, 0, 0);\n  text-align: center;\n  width: 30px;\n  height: 30px;\n  border-color: transparent;\n  left: 10px;\n}\n\nbutton:hover {\n  color: rgb(56, 56, 56);\n}\n\nbutton.playPauseButton {\n  top: 10px;\n}\n\nbutton.resetButton {\n  top: 45px;\n  border: 5px;\n}\n\nbutton.panButton {\n  top: 80px;\n  border: 5px;\n}\n\nbutton.orbitButton {\n  top: 80px;\n  border: 5px;\n}\n\n.scatterWidgetContainer {\n  position: relative;\n}\n\n.scatterWidgetCanvas {\n  position: absolute;\n}\n",""]);const o=i},645:t=>{t.exports=function(t){var e=[];return e.toString=function(){return this.map((function(e){var n=t(e);return e[2]?"@media ".concat(e[2]," {").concat(n,"}"):n})).join("")},e.i=function(t,n,r){"string"==typeof t&&(t=[[null,t,""]]);var i={};if(r)for(var o=0;o<this.length;o++){var a=this[o][0];null!=a&&(i[a]=!0)}for(var s=0;s<t.length;s++){var c=[].concat(t[s]);r&&i[c[0]]||(n&&(c[2]?c[2]="".concat(n," and ").concat(c[2]):c[2]=n),e.push(c))}},e}},252:(t,e,n)=>{n.r(e),n.d(e,{default:()=>g});var r=n(379),i=n.n(r),o=n(795),a=n.n(o),s=n(569),c=n.n(s),l=n(565),u=n.n(l),h=n(216),d=n.n(h),p=n(589),f=n.n(p),m=n(976),v={};v.styleTagTransform=f(),v.setAttributes=u(),v.insert=c().bind(null,"head"),v.domAPI=a(),v.insertStyleElement=d(),i()(m.Z,v);const g=m.Z&&m.Z.locals?m.Z.locals:void 0},379:t=>{var e=[];function n(t){for(var n=-1,r=0;r<e.length;r++)if(e[r].identifier===t){n=r;break}return n}function r(t,r){for(var o={},a=[],s=0;s<t.length;s++){var c=t[s],l=r.base?c[0]+r.base:c[0],u=o[l]||0,h="".concat(l," ").concat(u);o[l]=u+1;var d=n(h),p={css:c[1],media:c[2],sourceMap:c[3]};-1!==d?(e[d].references++,e[d].updater(p)):e.push({identifier:h,updater:i(p,r),references:1}),a.push(h)}return a}function i(t,e){var n=e.domAPI(e);return n.update(t),function(e){if(e){if(e.css===t.css&&e.media===t.media&&e.sourceMap===t.sourceMap)return;n.update(t=e)}else n.remove()}}t.exports=function(t,i){var o=r(t=t||[],i=i||{});return function(t){t=t||[];for(var a=0;a<o.length;a++){var s=n(o[a]);e[s].references--}for(var c=r(t,i),l=0;l<o.length;l++){var u=n(o[l]);0===e[u].references&&(e[u].updater(),e.splice(u,1))}o=c}}},569:t=>{var e={};t.exports=function(t,n){var r=function(t){if(void 0===e[t]){var n=document.querySelector(t);if(window.HTMLIFrameElement&&n instanceof window.HTMLIFrameElement)try{n=n.contentDocument.head}catch(t){n=null}e[t]=n}return e[t]}(t);if(!r)throw new Error("Couldn't find a style target. This probably means that the value for the 'insert' parameter is invalid.");r.appendChild(n)}},216:t=>{t.exports=function(t){var e=document.createElement("style");return t.setAttributes(e,t.attributes),t.insert(e),e}},565:(t,e,n)=>{t.exports=function(t){var e=n.nc;e&&t.setAttribute("nonce",e)}},795:t=>{t.exports=function(t){var e=t.insertStyleElement(t);return{update:function(n){!function(t,e,n){var r=n.css,i=n.media,o=n.sourceMap;i?t.setAttribute("media",i):t.removeAttribute("media"),o&&"undefined"!=typeof btoa&&(r+="\n/*# sourceMappingURL=data:application/json;base64,".concat(btoa(unescape(encodeURIComponent(JSON.stringify(o))))," */")),e.styleTagTransform(r,t)}(e,t,n)},remove:function(){!function(t){if(null===t.parentNode)return!1;t.parentNode.removeChild(t)}(e)}}}},589:t=>{t.exports=function(t,e){if(e.styleSheet)e.styleSheet.cssText=t;else{for(;e.firstChild;)e.removeChild(e.firstChild);e.appendChild(document.createTextNode(t))}}},28:(t,e)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.orbitIcon=e.panIcon=e.resetIcon=e.playIcon=e.pauseIcon=void 0,e.pauseIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="pause" class="svg-inline--fa fa-pause fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M144 479H48c-26.5 0-48-21.5-48-48V79c0-26.5 21.5-48 48-48h96c26.5 0 48 21.5 48 48v352c0 26.5-21.5 48-48 48zm304-48V79c0-26.5-21.5-48-48-48h-96c-26.5 0-48 21.5-48 48v352c0 26.5 21.5 48 48 48h96c26.5 0 48-21.5 48-48z"></path></svg>\n',e.playIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="play" class="svg-inline--fa fa-play fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M424.4 214.7L72.4 6.6C43.8-10.3 0 6.1 0 47.9V464c0 37.5 40.7 60.1 72.4 41.3l352-208c31.4-18.5 31.5-64.1 0-82.6z"></path></svg>\n',e.resetIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="redo-alt" class="svg-inline--fa fa-redo-alt fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M256.455 8c66.269.119 126.437 26.233 170.859 68.685l35.715-35.715C478.149 25.851 504 36.559 504 57.941V192c0 13.255-10.745 24-24 24H345.941c-21.382 0-32.09-25.851-16.971-40.971l41.75-41.75c-30.864-28.899-70.801-44.907-113.23-45.273-92.398-.798-170.283 73.977-169.484 169.442C88.764 348.009 162.184 424 256 424c41.127 0 79.997-14.678 110.629-41.556 4.743-4.161 11.906-3.908 16.368.553l39.662 39.662c4.872 4.872 4.631 12.815-.482 17.433C378.202 479.813 319.926 504 256 504 119.034 504 8.001 392.967 8 256.002 7.999 119.193 119.646 7.755 256.455 8z"></path></svg>\n',e.panIcon='\n<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="arrows-alt" class="svg-inline--fa fa-arrows-alt fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M352.201 425.775l-79.196 79.196c-9.373 9.373-24.568 9.373-33.941 0l-79.196-79.196c-15.119-15.119-4.411-40.971 16.971-40.97h51.162L228 284H127.196v51.162c0 21.382-25.851 32.09-40.971 16.971L7.029 272.937c-9.373-9.373-9.373-24.569 0-33.941L86.225 159.8c15.119-15.119 40.971-4.411 40.971 16.971V228H228V127.196h-51.23c-21.382 0-32.09-25.851-16.971-40.971l79.196-79.196c9.373-9.373 24.568-9.373 33.941 0l79.196 79.196c15.119 15.119 4.411 40.971-16.971 40.971h-51.162V228h100.804v-51.162c0-21.382 25.851-32.09 40.97-16.971l79.196 79.196c9.373 9.373 9.373 24.569 0 33.941L425.773 352.2c-15.119 15.119-40.971 4.411-40.97-16.971V284H284v100.804h51.23c21.382 0 32.09 25.851 16.971 40.971z"></path></svg>',e.orbitIcon='\n<?xml version="1.0" encoding="utf-8"?>\x3c!-- Generator: Adobe Illustrator 25.3.1, SVG Export Plug-In . SVG Version: 6.00 Build 0)  --\x3e<svg version="1.1" id="Layer_1" focusable="false" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"  x="0px" y="0px" viewBox="0 0 800 800" style="enable-background:new 0 0 800 800;" xml:space="preserve"><style type="text/css"> .st0{fill:none;stroke:#000000;stroke-width:80;stroke-miterlimit:10;} .st1{stroke:#000000;stroke-width:27;stroke-miterlimit:10;} .st2{fill:none;stroke:#000000;stroke-width:40;stroke-linecap:round;stroke-miterlimit:10;} .st3{fill:none;stroke:#000000;stroke-width:40;stroke-miterlimit:10;}</style><g> <path d="M179.7,542.3c-2.3,7-9.8,10.7-16.7,8.3c-41.1-14.5-74.7-32.5-99.8-53.6c-42.5-35.5-51.4-72.1-51.4-96.5  c0-25,9.3-62.3,53.5-98.3c26.3-21.4,61.4-39.6,104.3-54.1c6.9-2.3,14.4,1.5,16.5,8.5l16,51.6c2.1,6.7-1.6,13.9-8.3,16.2  c-32.8,11.2-59.6,24.9-78,39.9c-9,7.3-24,21.7-24,36.3c0,13.4,12.3,26.5,22.7,35.2c17.4,14.5,42.8,27.9,74,39.1  c6.6,2.4,10.1,9.6,7.9,16.2L179.7,542.3z"/></g><g> <path d="M401.9,587.4c-10.5,0-21-0.2-31.5-0.6c-7.2-0.3-12.9-6.4-12.5-13.7l2.7-53.9c0.4-7.1,6.4-12.6,13.5-12.3  c9.2,0.3,18.5,0.5,27.8,0.5c88.3,0,170.7-14.2,232.1-39.9c47.4-19.9,78-46.2,78-67s-30.6-47.1-78-67  c-61.3-25.7-143.7-39.9-232.1-39.9c-10.6,0-21.3,0.2-31.9,0.6c-7.1,0.3-13-5.2-13.4-12.3l-3-53.9c-0.4-7.2,5.2-13.4,12.4-13.7  c11.9-0.5,24-0.7,36-0.7c98.7,0,192.1,16.4,263,46.2c81.9,34.4,127,84.4,127,140.8s-45.1,106.4-127,140.8  C594,571,500.6,587.4,401.9,587.4z"/></g><g> <path d="M401.9,790.5c-56.4,0-106.4-45.1-140.8-127c-29.8-70.9-46.2-164.3-46.2-263s16.4-192.1,46.2-263  c34.4-81.9,84.4-127,140.8-127c29.4,0,71.6,12.5,110.4,69.8c4.2,6.1,2.3,14.5-4.1,18.4l-46.2,28.1c-5.8,3.5-13.5,2-17.4-3.6  c-14.6-20.8-29.8-32.6-42.7-32.6c-20.8,0-47.1,30.6-67,78c-25.7,61.3-39.9,143.7-39.9,232.1c0,88.3,14.2,170.7,39.9,232.1  c19.9,47.4,46.2,78,67,78c20.8,0,47.1-30.6,67-78c25.7-61.3,39.9-143.7,39.9-232.1c0-15.3-0.4-30.7-1.3-45.8  c-0.4-7.1,4.9-13.2,12-13.8l53.9-4c7.2-0.5,13.5,4.9,13.9,12.2c1,17,1.5,34.2,1.5,51.4c0,98.7-16.4,192.1-46.2,263  C508.3,745.4,458.3,790.5,401.9,790.5z"/></g><g> <path d="M511.3,175.3c-3.4,6.6-11.9,9.6-18.6,6.4L441,157.1l-37.5-17.8c-10.1-4.8-8.5-18.9,2.7-22.7L473,93.8l66.8-22.9  c11.1-3.8,21,6.3,16,16.3l-18.7,37L511.3,175.3z"/> <path d="M498.6,196.5c-3.9,0-7.9-0.8-11.7-2.6l-89.1-42.3c-9.5-4.5-15-13.7-14.4-23.9c0.6-10.8,7.9-20,18.5-23.7l133.6-45.7  c10.6-3.6,22-0.7,29.1,7.4c6.8,7.8,8,18.4,3.3,27.7l-44.5,88.1C518.5,190.9,508.7,196.5,498.6,196.5z M412.7,128.7l85.8,40.7  c0.2,0,0.7-0.1,0.8-0.3l42.8-84.7L412.7,128.7z"/></g><g> <path d="M339.2,540.4c-3.9-6.3-2-15.2,4.3-19.2l47.8-31.3l34.7-22.7c9.4-6.1,20.5,2.6,18,14.1l-15.2,68.9L413.5,619  c-2.5,11.5-16.3,14.7-22.2,5.2L369.4,589L339.2,540.4z"/> <path d="M400.9,643.3c-8.5,0-16.4-4.4-21.1-12l-52-83.8c-7.8-12.6-4.1-29.5,8.3-37.7l82.5-54.1c8.8-5.7,19.5-5.7,27.9,0.2  c8.9,6.1,13,17.1,10.6,28.1l-30.5,137.9c-2.4,10.9-10.8,19.2-21.5,21C403.8,643.2,402.3,643.3,400.9,643.3z M350.7,533.3l50.1,80.6  l29.5-133.5l-79.4,52C350.8,532.6,350.7,533.1,350.7,533.3z"/></g></svg>\n'},467:(t,e,n)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.ScatterWidget=void 0;var r=n(96);Object.defineProperty(e,"ScatterWidget",{enumerable:!0,get:function(){return r.ScatterWidget}})},96:function(t,e,n){var r=this&&this.__createBinding||(Object.create?function(t,e,n,r){void 0===r&&(r=n),Object.defineProperty(t,r,{enumerable:!0,get:function(){return e[n]}})}:function(t,e,n,r){void 0===r&&(r=n),t[r]=e[n]}),i=this&&this.__setModuleDefault||(Object.create?function(t,e){Object.defineProperty(t,"default",{enumerable:!0,value:e})}:function(t,e){t.default=e}),o=this&&this.__importStar||function(t){if(t&&t.__esModule)return t;var e={};if(null!=t)for(var n in t)"default"!==n&&Object.prototype.hasOwnProperty.call(t,n)&&r(e,t,n);return i(e,t),e};Object.defineProperty(e,"__esModule",{value:!0}),e.ScatterWidget=void 0;var a=o(n(212)),s=n(681),c=n(148),l=n(886),u=n(28);n(252);var h=function(){function t(t,e,n){this.canvas=document.createElement("canvas"),this.clock=new a.Clock,this.pointsBuffers=[],this.minPointSize=.02,this.width=e,this.height=n,this.addContainerElement(t),this.addCanvas(),this.addScene(),this.addRenderer(),this.addControls()}return t.prototype.constructPlot=function(){var t=this;this.colMeans=s.getColMeans(this.dataset),this.pointColours=this.coloursToBufferAttribute(this.mapping.colour),this.pickingColours=this.getPickingColours();var e=new a.BufferGeometry,n=Math.pow(this.dataset.length,-1/3),r=this.getShaderOpts(n,this.dim),i=new a.ShaderMaterial(r),o=this.getPointsBuffer(0,this.config.center);e.setAttribute("position",o),this.config.cacheFrames&&this.pointsBuffers.push(o),this.points=new a.Points(e,i),this.points.geometry.setAttribute("color",this.pointColours),this.scene.add(this.points);var c=new a.BufferGeometry,l=new a.LineBasicMaterial({color:0,linewidth:1}),u=this.getAxisLinesBuffer(0);c.setAttribute("position",u),this.axisSegments=new a.LineSegments(c,l),this.scene.add(this.axisSegments),this.addOrbitControls(),this.pickingTexture=new a.WebGLRenderTarget(this.width,this.height),this.container.addEventListener("mousemove",(function(e){return t.getPointIndicesFromBoxSelection(e)})),this.clock=new a.Clock,this.time=0,this.oldFrame=-1,this.isPaused=!1},t.prototype.resize=function(t,e){var n=t/e;this.canvas.width=t,this.canvas.height=e,3==this.dim?this.camera.aspect=n:(this.camera.left=-1*n,this.camera.right=1*n,this.camera.top=1,this.camera.bottom=-1),this.camera.updateProjectionMatrix(),this.renderer.setSize(t,e),this.pickingTexture=new a.WebGLRenderTarget(t,e)},t.prototype.renderValue=function(t){this.config=t.config,this.dataset=t.dataset,this.projectionMatrices=t.projectionMatrices,this.dim=t.projectionMatrices[0][0].length,3==this.dim?this.multiply=s.multiply3:this.multiply=s.multiply2,this.addCamera(this.dim),this.mapping=t.mapping,this.constructPlot(),this.animate()},t.prototype.addContainerElement=function(t){t.className="scatterWidgetContainer",this.container=t},t.prototype.addScene=function(){var t=new a.Scene;t.background=new a.Color(268435455);var e=new a.AmbientLight(4210752);t.add(e),this.scene=t},t.prototype.addCanvas=function(){var t=document.createElement("canvas");t.width=this.width,t.height=this.height,t.id=this.container.id+"-canvas",t.className="scatterWidgetCanvas",this.container.appendChild(t),this.canvas=t},t.prototype.addCamera=function(t){var e,n=this.width/this.height;3==t?(e=new a.PerspectiveCamera(45,n,.01,1e3)).position.setZ(4):((e=new a.OrthographicCamera(-1*n,1*n,1,-1,-1e3,1e3)).position.setY(4),e.up.set(0,-1,0)),this.camera=e},t.prototype.addRenderer=function(){var t=new a.WebGLRenderer({canvas:this.canvas});t.setPixelRatio(window.devicePixelRatio),t.setSize(this.width,this.height),this.renderer=t},t.prototype.addOrbitControls=function(){var t=new l.OrbitControls(this.camera,this.renderer.domElement);2==this.dim&&(t.minPolarAngle=Math.PI,t.maxPolarAngle=Math.PI),this.orbitControls=t},t.prototype.getShaderOpts=function(t,e){return 2==e?{uniforms:{size:{value:Math.max(t,this.minPointSize)},zoom:{value:this.camera.zoom}},vertexShader:c.VERTEX_SHADER_2D,fragmentShader:c.FRAGMENT_SHADER}:{uniforms:{size:{value:Math.max(t,this.minPointSize)}},vertexShader:c.VERTEX_SHADER_3D,fragmentShader:c.FRAGMENT_SHADER}},t.prototype.getPointsBuffer=function(t,e){var n=this.multiply(this.dataset,this.projectionMatrices[t]);if(e){var r=this.multiply(this.colMeans,this.projectionMatrices[t]);n=s.centerColumns(n,r)}var i=new Float32Array([].concat.apply([],n));return new a.BufferAttribute(i,3)},t.prototype.getAxisLinesBuffer=function(t){var e,n=this.projectionMatrices[t];return 3==this.dim?e=n.map((function(t){return[0,0,0].concat(t)})):2==this.dim&&(e=n.map((function(t){return[0,0,0,t[0],0,t[1]]}))),new a.BufferAttribute(new Float32Array([].concat.apply([],e)),3)},t.prototype.coloursToBufferAttribute=function(t){var e=new a.Color,n=new Float32Array(3*this.dataset.length);if(t.length>0)for(var r=0,i=0;i<t.length;i++)r=3*i,e.set(t[i]),n[r]=e.r,n[r+1]=e.g,n[r+2]=e.b;return new a.BufferAttribute(n,3)},t.prototype.getPickingColours=function(){for(var t=new Float32Array(3*this.dataset.length),e=0,n=1;n<=this.dataset.length;n++)t[e]=0,t[e+1]=Math.floor(n/255)/255,t[e+2]=n%255/255,e+=3;return new a.BufferAttribute(t,3)},t.prototype.addControls=function(){var t=this;this.addButton("playPause","Play / Pause",u.pauseIcon,(function(){return t.setIsPaused(!t.getIsPaused())})),this.addButton("reset","Restart tour",u.resetIcon,(function(){return t.resetClock()})),this.addButton("pan","Switch to pan controls",u.panIcon,(function(){return t.setControlType("PAN"==t.controlType?"ORBIT":"PAN")}))},t.prototype.addButton=function(t,e,n,r){var i=document.createElement("button");i.innerHTML=n,i.title=e,i.className=t+"Button",i.onclick=function(){return r()},this.container.appendChild(i)},t.prototype.getPointIndicesFromBoxSelection=function(t){var e=new Uint8Array(4),n=this,r=n.pickingTexture,i=(n.renderer,n.camera,n.scene,{x:t.clientX,y:t.clientY,width:1,height:1});this.renderer.readRenderTargetPixels(r,i.x,r.height-i.y,i.width,i.height,e);var o=e[0]<<16|e[1]<<8|e[2];16777215!=o&&console.log(o)},t.prototype.animate=function(){var t=this,e=this.clock.getDelta();this.getIsPaused()||(this.time+=e),this.time>=this.config.duration&&(this.time=0);var n=Math.floor(this.time*this.config.fps);if(n!=this.oldFrame){var r=void 0;null==this.pointsBuffers[n]?(r=this.getPointsBuffer(n,this.config.center),this.config.cacheFrames&&(this.pointsBuffers[n]=r)):r=this.pointsBuffers[n],this.points.geometry.setAttribute("position",r),this.points.geometry.attributes.position.needsUpdate=!0,this.axisSegments.geometry.setAttribute("position",this.getAxisLinesBuffer(n)),this.axisSegments.geometry.attributes.position.needsUpdate=!0,this.oldFrame=n}2==this.dim&&(this.points.material.uniforms.zoom.value=this.camera.zoom),this.points.geometry.setAttribute("color",this.pickingColours),this.renderer.setRenderTarget(this.pickingTexture),this.renderer.render(this.scene,this.camera),this.renderer.setRenderTarget(null),this.points.geometry.setAttribute("color",this.pointColours),this.renderer.render(this.scene,this.camera),requestAnimationFrame((function(){return t.animate()}))},t.prototype.getIsPaused=function(){return this.isPaused},t.prototype.setIsPaused=function(t){this.isPaused=t;var e=this.container.querySelector(".playPauseButton");t?e.innerHTML=u.playIcon:(this.animate(),e.innerHTML=u.pauseIcon)},t.prototype.setControlType=function(t){var e="ORBIT"==t?".orbitButton":".panButton",n=this.container.querySelector(e);"ORBIT"==t?(n.innerHTML=u.panIcon,n.title="Switch to pan controls",n.className="panButton",this.orbitControls.mouseButtons={LEFT:a.MOUSE.ROTATE,MIDDLE:a.MOUSE.DOLLY,RIGHT:a.MOUSE.PAN}):(n.innerHTML=u.orbitIcon,n.title="Switch to orbit controls",n.className="orbitButton",this.orbitControls.mouseButtons={LEFT:a.MOUSE.PAN,MIDDLE:a.MOUSE.DOLLY,RIGHT:a.MOUSE.ROTATE}),this.controlType=t},t.prototype.resetClock=function(){this.time=0},t}();e.ScatterWidget=h},148:(t,e)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.VERTEX_SHADER_2D=e.VERTEX_SHADER_3D=e.FRAGMENT_SHADER=void 0,e.FRAGMENT_SHADER="\nvarying float vSize;\nvarying vec3 vColor;\n\nvoid main(){\n    gl_FragColor = vec4( vColor, 1.0 );\n\n    // make points circular\n    float distance = length(2.0 * gl_PointCoord - 1.0);\n    if (distance > 1.0) {\n        discard;\n    }\n}\n",e.VERTEX_SHADER_3D="\nuniform float size;\nattribute vec3 color;\nvarying vec3 vColor;\n\nvoid main(){\n    vColor=color;\n    vec4 mvPosition = modelViewMatrix * vec4( position, 1.0);\n    gl_Position = projectionMatrix * mvPosition;\n    gl_PointSize = 200.0 * size / -mvPosition.z;\n}\n",e.VERTEX_SHADER_2D="\nuniform float size;\nuniform float zoom;\n\nattribute vec3 color;\nvarying vec3 vColor;\n\nvoid main(){\n    vColor=color;\n    vec4 mvPosition = modelViewMatrix * vec4( position, 1.0);\n    gl_Position = projectionMatrix * mvPosition;\n    gl_PointSize = 100.0 * size * sqrt(zoom);\n}\n"},681:(t,e)=>{Object.defineProperty(e,"__esModule",{value:!0}),e.centerColumns=e.getColMeans=e.multiply2=e.multiply3=void 0,e.multiply3=function(t,e){for(var n=t.length,r=t[0].length,i=new Array(n),o=0;o<n;++o){var a=new Array(3);i[o]=a;for(var s=t[o],c=0;c<3;++c){for(var l=0,u=0;u<r;++u)l+=s[u]*e[u][c];a[c]=l}}return i},e.multiply2=function(t,e){for(var n=t.length,r=t[0].length,i=new Array(n),o=0;o<n;++o){var a=new Array(3);i[o]=a;for(var s=t[o],c=0;c<2;c++){for(var l=0,u=0;u<r;++u)l+=s[u]*e[u][c];a[2*c]=l}a[1]=0}return i},e.getColMeans=function(t){var e=t.length;return[t.reduce((function(t,e){return t.map((function(t,n){return t+e[n]}))})).map((function(t){return t/e}))]},e.centerColumns=function(t,e){return t.map((function(t){return t.map((function(t,n){return t-e[0][n]}))}))}}},n={};function r(t){var i=n[t];if(void 0!==i)return i.exports;var o=n[t]={id:t,exports:{}};return e[t].call(o.exports,o,o.exports,r),o.exports}r.m=e,t=[],r.O=(e,n,i,o)=>{if(!n){var a=1/0;for(l=0;l<t.length;l++){for(var[n,i,o]=t[l],s=!0,c=0;c<n.length;c++)(!1&o||a>=o)&&Object.keys(r.O).every((t=>r.O[t](n[c])))?n.splice(c--,1):(s=!1,o<a&&(a=o));s&&(t.splice(l--,1),e=i())}return e}o=o||0;for(var l=t.length;l>0&&t[l-1][2]>o;l--)t[l]=t[l-1];t[l]=[n,i,o]},r.n=t=>{var e=t&&t.__esModule?()=>t.default:()=>t;return r.d(e,{a:e}),e},r.d=(t,e)=>{for(var n in e)r.o(e,n)&&!r.o(t,n)&&Object.defineProperty(t,n,{enumerable:!0,get:e[n]})},r.o=(t,e)=>Object.prototype.hasOwnProperty.call(t,e),r.r=t=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},(()=>{var t={499:0};r.O.j=e=>0===t[e];var e=(e,n)=>{var i,o,[a,s,c]=n,l=0;for(i in s)r.o(s,i)&&(r.m[i]=s[i]);if(c)var u=c(r);for(e&&e(n);l<a.length;l++)o=a[l],r.o(t,o)&&t[o]&&t[o][0](),t[a[l]]=0;return r.O(u)},n=self.webpackChunkscatter_widget=self.webpackChunkscatter_widget||[];n.forEach(e.bind(null,0)),n.push=e.bind(null,n.push.bind(n))})();var i=r.O(void 0,[841],(()=>r(467)));i=r.O(i),scatter_widget=i})();
