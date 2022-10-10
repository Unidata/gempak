// document.body.onload = addElement;

// function addElement() {
//   const head = document.head;
//   const link = document.createElement("link");
//   link.type = "text/css";
//   link.rel = "stylesheet";
//   link.href = "/gempak/add_in.css";
//   head.appendChild(link);

//   const p = document.createElement("p");
//   const warnDiv = document.createElement("div");
//   warnDiv.className = "warnbox";

//   const warnString = '<strong>NOTE:</strong> Unidata no longer supports the GEMPAK \
//   software package. This documentation, which corresponds to GEMPAK version 7.5.1 \
//   (released in 2019) is provided as a convenience for the community.<br><br> \
//   Please visit the <a href="https://github.com/Unidata/gempak">GEMPAK Github Repository</a> \
//   for more current information.'; 

//   p.innerHTML = warnString;
//   warnDiv.insertBefore(p, null);
//   document.body.insertBefore(warnDiv, document.body.firstChild);
// }