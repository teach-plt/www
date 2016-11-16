function generateSectionsMenu() {
  var h1_list = document.getElementsByTagName("H1");

  var menu_div = document.createElement("DIV");
  menu_div.style.position = "fixed";
  menu_div.style.top = "0em";
  menu_div.style.left = "0em";
  menu_div.style.width = setMenyDivWidth();
  menu_div.style.backgroundColor = "#888";
  menu_div.style.borderRadius = "1em";
  menu_div.style.margin = "0.5em";

  var menu = document.createElement("UL");
  menu.style.listStyleType = "none";
  menu.style.padding = "0em 0.5em 0.5em 0.5em";
  // // Andreas, 2016-11-16, I like it better without heading.
  // // The menu should be self-explaining.
  // var header = createLI();
  // header.textContent = "Sections";
  // header.style.textDecoration = "underline";
  // menu.appendChild(header);

  menu_div.appendChild(menu);

  for (var i = 1; i < h1_list.length; i++) {
    var bookmark = "section"+i;
    var label = createLI();
    var anchor = document.createElement("a");
    h1_list[i].id = bookmark;
    anchor.href = "#"+bookmark;
    anchor.textContent = h1_list[i].textContent;
    anchor.style.textDecoration = "none";
    label.appendChild(anchor);
    menu.appendChild(label);
  }
  document.body.insertBefore(menu_div, document.body.firstChild);

  function createLI() {
    var label = document.createElement("li");
    label.style.margin = "0.8em 0em 0.8em 0em";
    return label;
  }

  addEvent('resize', () => setMenyDivWidth());

  function setMenyDivWidth() {
    menu_div.style.width = document.body.getBoundingClientRect().left - 2*0.5*getEmDouble();
  }

  function getEmDouble() {
    return parseFloat(getComputedStyle(document.body).fontSize);
  }
}

function addScrollListener() {
  addEvent("keydown", function(e) {
    if (e.which == 74) {
        window.scrollBy(0,20);
    }
    else if (e.which == 75) {
        window.scrollBy(0,-20);
    }
  }, false);
}


function init() {
  generateSectionsMenu();
  addScrollListener()
}


function addEvent(eventStr, listener) {
  if (window.addEventListener) { // W3C standard
    window.addEventListener(eventStr, listener); // NB **not** 'onload'
  } else if (window.attachEvent) { // Microsoft
    window.attachEvent(eventStr, init);
  }
}


if (document.readyState === "complete") {
  init();
} else
if (window.addEventListener) { // W3C standard
  window.addEventListener("load", init); // NB **not** 'onload'
} else if (window.attachEvent) { // Microsoft
  window.attachEvent("onload", init);
}
