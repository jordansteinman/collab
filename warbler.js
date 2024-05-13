/*JavaScript Codes */

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
/* java done by Rylee*/
/*java needed on the responsiv navbar*/
/*Toggle between adding and removing the "responsive" class to topnav when the user clicks on the icon*/
function myFunction() {
  var x = document.getElementById("myTopnav");
  if (x.className === "topnav") {
    x.className += " responsive";
  } else {
    x.className = "topnav";
  }
}
/* Rylee ends*/

/* Javascript to toggle between two images inspired by w3 schools tutorial https://www.w3schools.com/html/tryit.asp?filename=tryhtml_script_attribute Maya Ivimey*/
function pickles(change) {
 	 var pic;
  	if (change == 0) {
   	 pic = "Images/7pickles.jpg"
  	} else {
    	pic = "Images/9peicesofgum.jpg"
 	 }
  	document.getElementById('myImage').src = pic;
	}
function grapes(change) {
 	 var pic;
  	if (change == 0) {
   	 pic = "Images/grapes.png"
  	} else {
    	pic = "Images/apples.jpg"
 	 }
  	document.getElementById('myImage2').src = pic;
	}
