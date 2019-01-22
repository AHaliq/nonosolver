//const URL = "http://35.247.172.11:8000/"
const URL = "http://localhost:8000/";
var tests;
var solver = 1;
// variables ------------------------------------

document.getElementById("requery").addEventListener("click", e => {
    showText("",false);
    document.getElementById("controls").style.display = "block";
    e.srcElement.style.display = "none";
});
document.getElementById("textArRow").addEventListener("keydown", resetTestSel);
document.getElementById("textArCol").addEventListener("keydown", resetTestSel);

document.getElementById("ssel").addEventListener("change",
    e => solver = e.target.options.selectedIndex + 1);
document.getElementById("tsel").addEventListener("change",
    e => showTestCase(tests[e.target.options.selectedIndex - 1][1]));
// ui listeners ---------------------------------

function showTestCase(str) {
    s = str.split("\ne\n");
    document.getElementById("textArRow").value = s[0];
    document.getElementById("textArCol").value = s[1];
}

function resetTestSel() {
    document.getElementById("tsel").selectedIndex = "0";
}

function showText(str) {
    document.getElementById("outAr").innerHTML = str;
}
// util functions -------------------------------

document.getElementById("subBtn").addEventListener("click", submitFunc);
function submitFunc() {
    document.getElementById("controls").style.display = "none";
    document.getElementById("requery").style.display = "block";
    showText("querying solution<br><i>if unresponsive try 'solve without guessing'</i>", false);
    // set control ui

    var clean = str => str
        .replace(/[^0-9 \n]/g, "")
        .replace(/\n/g, "n")
        .replace(/\s+/g, " ")
        .replace(/n/g,"\n");

    var puzzle = `${clean(document.getElementById("textArRow").value)}\ne\n${clean(document.getElementById("textArCol").value)}\ne\n`;
    showTestCase(puzzle);
    
    fetch(`${URL}solve`,{
        headers: { "Content-type": "application/x-www-form-urlencoded" },
        body: `text=${puzzle}&solv=${solver}`,
        method:"POST"
    })
    .then(data => data.json())
    .then(res => showText(
        res.length === 0 ? "puzzle has no solution" : 
        res.reduce((a,c,i) => a + (solver == 1 ? `Soln ${i+1}: <br /><br />` : "") +
            c.reduce((f,g) => f + g.reduce((b,d) => b + d) + "<br />","") + "<br />", "")
        , false))
    .catch(err => console.error(err));
    // make request
}

// request solution -----------------------------

fetch(`${URL}tests`, { method:"GET" })
.then(data => data.json())
.then(res => {
    tests = res;
    document.getElementById("tsel").innerHTML =
        res.reduce((a,c) => a + `<option value="${c[0]}">${c[0]}</option>`,
            `<option disabled selected>load test case</option>`);
})
.catch(err => console.error(err));

// load test cases ------------------------------