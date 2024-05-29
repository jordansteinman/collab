/*Expanding Buttons*/
var acc = document.getElementsByClassName("ExpandingButton");
var i;

for (i = 0; i < acc.length; i++) {
  acc[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var Article = this.nextElementSibling;
    if (Article.style.display === "block") {
      Article.style.display = "none";
    } else {
      Article.style.display = "block";
    }
  });
}

/*Java needed on the responsive navbar*/
/*Toggle between adding and removing the "responsive" class to topnav when the user clicks on the icon*/
function myFunction() {
  var x = document.getElementById("myTopnav");
  if (x.className === "topnav") {
    x.className += " responsive";
  } else {
    x.className = "topnav";
  }
}

//back-to-top button to bring back to the top once clicked https://www.w3schools.com/howto/howto_js_scroll_to_top.asp
function topFunction() {
    document.body.scrollTop = 0; // Safari
    document.documentElement.scrollTop = 0; // Chrome, Firefox, Opera
  }

//back-to-top button appear once scrolled (https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_onscroll2)
window.onscroll = function() {scrollTopButton()};
function scrollTopButton() {
  if (document.documentElement.scrollTop > 10) {
    document.getElementById("topbutton").className = "top-show"; //changes class to top-show where display:block
  } else {
    document.getElementById("topbutton").className = "hide"; //keeps class to top-hide where display:none
  }
}
