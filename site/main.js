const URL = "http://35.247.172.11:8000/"
//const URL = "http://localhost:8000/";

function showText(str, area = true) {
    if(area) {
        document.getElementById("textAr").value = str;
    }else {
        document.getElementById("outAr").innerHTML = str;
    }
}
// print util
document.getElementById("subBtn").addEventListener("click", submitFunc);
function submitFunc() {
    showText("querying solution<br>(if unresponsive try 'solve without guessing')", false);
    
    fetch(`${URL}solve`,{
        headers:{
            "content-type": "application/x-www-form-urlencoded",
            "cache-control": "no-cache",
            "Postman-Token": "cb805568-b265-4674-a928-9c4e9c200bbd"
        },
        body: `text=${document.getElementById("textAr").value.replace(/\n/g, "nn")}&solv=${solver}`,
        method:"POST"
    })
    .then(data => data.json())
    .then(res => showText(
        res.length === 0 ? "puzzle has no solution" : 
        res.reduce((a,c,i) => a + (solver == 1 ? `Soln ${i}: <br /><br />` : "") +
            c.reduce((f,g) => f + g.reduce((b,d) => b + d) + "<br />","") + "<br />", "")
        , false))
    .catch(err => console.log(err));
}

// request solution -----------------------------

var tests;
var solver = 1;
document.getElementById("textAr").addEventListener("keydown", e => document.getElementById("tsel").selectedIndex = "0");
document.getElementById("ssel").addEventListener("change", e => solver = e.target.options.selectedIndex + 1);
document.getElementById("tsel").addEventListener("change",
    e => showText(tests[e.target.options.selectedIndex - 1][1]));
// print test case to text area

fetch(`${URL}tests`, {
    headers:{
        "content-type": "application/x-www-form-urlencoded",
        "cache-control": "no-cache",
        "Postman-Token": "a3415066-52cf-4b4c-b66c-df15efcb8ea0"
    },
    method:"GET"
})
.then(data => data.json())
.then(res => {
    tests = res;
    document.getElementById("tsel").innerHTML =
        res.reduce((a,c) => a + `<option value="${c[0]}">${c[0]}</option>`,
            `<option disabled selected>load test case</option>`);
}) // put into select
.catch(err => console.log(err));

// load test cases ------------------------------