function cbn(frame) {
document.getElementById("cbn_step").innerHTML = frame;
if(frame == 0) {
document.getElementById("cbn_heap").innerHTML = "";
document.getElementById("cbn_term").innerHTML = "<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">twice</span>";
}
if(frame == 1) {
document.getElementById("cbn_heap").innerHTML = "";
document.getElementById("cbn_term").innerHTML = "<span style=\"color: darkblue;\">comp</span>&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">comp</span>";
}
if(frame == 2) {
document.getElementById("cbn_heap").innerHTML = "";
document.getElementById("cbn_term").innerHTML = "(\\<span style=\"font-style: italic;\">g</span>&nbsp;<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;(<span style=\"font-style: italic;\">g</span>&nbsp;<span style=\"font-style: italic;\">x</span>))&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: beta&nbsp;reduction";
}
if(frame == 3) {
document.getElementById("cbn_heap").innerHTML = "";
document.getElementById("cbn_term").innerHTML = "(\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;(<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"font-style: italic;\">x</span>))&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: beta&nbsp;reduction";
}
if(frame == 4) {
document.getElementById("cbn_heap").innerHTML = "";
document.getElementById("cbn_term").innerHTML = "<span style=\"color: darkblue;\">twice</span>&nbsp;(<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>)&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">twice</span>";
}
if(frame == 5) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>";
document.getElementById("cbn_term").innerHTML = "<span style=\"color: darkblue;\">comp</span>&nbsp;f_0&nbsp;f_0&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">comp</span>";
}
if(frame == 6) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>";
document.getElementById("cbn_term").innerHTML = "(\\<span style=\"font-style: italic;\">g</span>&nbsp;<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;f_0&nbsp;(<span style=\"font-style: italic;\">g</span>&nbsp;<span style=\"font-style: italic;\">x</span>))&nbsp;f_0&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: beta&nbsp;reduction";
}
if(frame == 7) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>";
document.getElementById("cbn_term").innerHTML = "(\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;f_0&nbsp;(f_0&nbsp;<span style=\"font-style: italic;\">x</span>))&nbsp;2";
document.getElementById("cbn_status").innerHTML = "next step: beta&nbsp;reduction";
}
if(frame == 8) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;<span style=\"color: darkblue;\">double</span>";
document.getElementById("cbn_term").innerHTML = "f_0&nbsp;(f_0&nbsp;2)";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">twice</span>&nbsp;in&nbsp;[f_0]";
}
if(frame == 9) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;<span style=\"color: darkblue;\">comp</span>&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"color: darkblue;\">double</span>";
document.getElementById("cbn_term").innerHTML = "f_0&nbsp;(f_0&nbsp;2)";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">comp</span>&nbsp;in&nbsp;[f_0]";
}
if(frame == 10) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;(\\<span style=\"font-style: italic;\">g</span>&nbsp;<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"font-style: italic;\">g</span>&nbsp;<span style=\"font-style: italic;\">x</span>))&nbsp;<span style=\"color: darkblue;\">double</span>";
document.getElementById("cbn_term").innerHTML = "f_0&nbsp;(f_0&nbsp;2)";
document.getElementById("cbn_status").innerHTML = "next step: beta&nbsp;reduction&nbsp;in&nbsp;[f_0]";
}
if(frame == 11) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)";
document.getElementById("cbn_term").innerHTML = "f_0&nbsp;(f_0&nbsp;2)";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;f_0";
}
if(frame == 12) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)";
document.getElementById("cbn_term").innerHTML = "<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;(f_0&nbsp;2))";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">double</span>";
}
if(frame == 13) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(f_0&nbsp;2)";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;in&nbsp;[x_1]";
}
if(frame == 14) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;x_2&nbsp;+&nbsp;x_2<br>x_2&nbsp;=&nbsp;f_0&nbsp;2";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;f_0&nbsp;in&nbsp;[x_1,&nbsp;x_2]";
}
if(frame == 15) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;x_2&nbsp;+&nbsp;x_2<br>x_2&nbsp;=&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;2)";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;in&nbsp;[x_1,&nbsp;x_2]";
}
if(frame == 16) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;x_2&nbsp;+&nbsp;x_2<br>x_2&nbsp;=&nbsp;x_3&nbsp;+&nbsp;x_3<br>x_3&nbsp;=&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;2";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: apply&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;in&nbsp;[x_1,&nbsp;x_2,&nbsp;x_3]";
}
if(frame == 17) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;x_2&nbsp;+&nbsp;x_2<br>x_2&nbsp;=&nbsp;x_3&nbsp;+&nbsp;x_3<br>x_3&nbsp;=&nbsp;2&nbsp;+&nbsp;2";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: delta:&nbsp;2&nbsp;+&nbsp;2&nbsp;in&nbsp;[x_1,&nbsp;x_2,&nbsp;x_3]";
}
if(frame == 18) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;x_2&nbsp;+&nbsp;x_2<br>x_2&nbsp;=&nbsp;x_3&nbsp;+&nbsp;x_3<br>x_3&nbsp;=&nbsp;4";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: delta:&nbsp;4&nbsp;+&nbsp;4&nbsp;in&nbsp;[x_1,&nbsp;x_2]";
}
if(frame == 19) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;x_2&nbsp;+&nbsp;x_2<br>x_2&nbsp;=&nbsp;8<br>x_3&nbsp;=&nbsp;4";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: delta:&nbsp;8&nbsp;+&nbsp;8&nbsp;in&nbsp;[x_1]";
}
if(frame == 20) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;16<br>x_2&nbsp;=&nbsp;8<br>x_3&nbsp;=&nbsp;4";
document.getElementById("cbn_term").innerHTML = "x_1&nbsp;+&nbsp;x_1";
document.getElementById("cbn_status").innerHTML = "next step: delta:&nbsp;16&nbsp;+&nbsp;16";
}
if(frame == 21) {
document.getElementById("cbn_heap").innerHTML = "f_0&nbsp;=&nbsp;\\<span style=\"font-style: italic;\">x</span>&nbsp;-&gt;&nbsp;<span style=\"color: darkblue;\">double</span>&nbsp;(<span style=\"color: darkblue;\">double</span>&nbsp;<span style=\"font-style: italic;\">x</span>)<br>x_1&nbsp;=&nbsp;16<br>x_2&nbsp;=&nbsp;8<br>x_3&nbsp;=&nbsp;4";
document.getElementById("cbn_term").innerHTML = "32";
document.getElementById("cbn_status").innerHTML = "whnf";
}
}
var cbn_frame = 0;
function cbnNext() {
cbn(++cbn_frame);
}
function cbnPrev() {
cbn(--cbn_frame);
}
cbn(cbn_frame);
