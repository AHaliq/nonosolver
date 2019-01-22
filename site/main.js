//const URL = "http://35.247.172.11:8000/"
const URL = "http://localhost:8000/";
/** file name test case pair */
var tests;
/** solver type 1-all,2-first,3-no guess */
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

function clean(str) {
    return str.replace(/[^0-9 \n]/g, "").replace(/\n/g, "n").replace(/\s+/g, " ").replace(/n/g,"\n");
}

function showText(str) {
    document.getElementById("outAr").innerHTML = str;
}
// util functions -------------------------------

function printCanvas(res, puz) {
    /** tile dimensions */
    const bwdt = 16;
    /** font size for hints */
    const fnt = 10.7;
    /** cross ratio to tile */
    const crat = 0.4;
    /** board margin */
    const bmar = 20;
    /** line weight for thick lines */
    const wthk = 1.3;
    /** line weight for thin lines */
    const wthn = 0.4;
    /** thick line after every gridw tiles */
    const gridw = 5;
    // drawing constants

    /** hints */
    var hs = puz.split("\ne\n").map(s => s.split("\n").map(v => v === "" ? [] : v.split(" ")));
    /** [longest row hint amount in a row, longest col hint amount in a col] */
    var hn = hs.map(s => s.reduce((a,c) => Math.max(a, c.length), 0));
    /** [puzzle height, puzzle width] */
    var dim = hs.map(s => s.length);
    /** [board tile height, board tile width] */
    var bdn = dim.map((v,i) => parseInt(v) + parseInt(hn[1-i]));
    // puzzle dimensions

    var bdr = bdn.map(v => v * bwdt);
    var bdrm = bdr.map(v => v + bmar * 2);
    // render dimensions

    function printBoard(c, i) {
        const top = bmar + bdrm[0] * i;
        const left = bmar;
        const btm = top + bdr[0];
        const right = left + bdr[1];
        const itop = top + hn[1] * bwdt;
        const ileft = left + hn[0] * bwdt;
        var str = `<g>`;
        str = range(0,bdn[1]).reduce((a,c) =>
            ((x,l) => a + drawLine(x, l ? top : itop, x, btm,
                l && (c - hn[0]) % gridw === 0 || c === 0 || c === bdn[1] ? wthk : wthn))
            (left+bwdt*c, c>=hn[0]), str);
        str = range(0,bdn[0]).reduce((a,c) =>
            ((y,t) => a + drawLine(t ? left : ileft, y, right, y,
                t && (c - hn[1]) % gridw === 0 || c === 0 || c === bdn[0] ? wthk : wthn))
            (top+bwdt*c, c>=hn[1]),str);
        // print grid
        
        str = hs[0].reduce((a1,c1,ri) => c1.reduce((a2,c2,hi) =>
            a2 + drawHint(left + (hn[0] - c1.length + hi + 0.5) * bwdt, itop + (ri + 0.75) * bwdt,c2,fnt), a1), str);
        str = hs[1].reduce((a1,c1,ci) => c1.reduce((a2,c2,hi) =>
            a2 + drawHint(ileft + (ci + 0.5) * bwdt, top + (hn[1] - c1.length + hi + 0.75) * bwdt,c2,fnt), a1), str);
        // print hints
        
        str = c.reduce((a1,r,y) => r.reduce((a2,t,x) =>
            a2 + (t === '*' ? "" : drawTile(ileft + x * bwdt, itop + y * bwdt, bwdt, crat, t === 'O')), a1), str);
        // print tiles
        str += `</g>`;
        return str;
    }
    
    var str = `<svg width="${bdrm[1]}" height="${bdrm[0]*res.length}">`;
    str = res.reduce((a,c,i) => a + printBoard(c, i), str);
    str += `</svg>`;
    showText(str);
}

function range(a,b) {
    return Array(b-a+1).fill(a).map((v,i) => v + i);
}

function drawTile(x,y,w,r,solid = true) {
    var m = (1-r)* 0.5 * w
    var t = y + m, l = x + m, b = y + w - m, r = x + w - m;
    return solid ? `<rect x="${x}" y="${y}" width="${w}" height="${w}" style="fill: rgba(0, 0, 0, 0.6)" />` :
    `<path d="M ${l} ${t} L ${r} ${b} M ${l} ${b} L ${r} ${t} Z" style="stroke: silver; stroke-width: 2px"></path>`;
}

function drawHint(x,y,t,s = 10) {
    return `<text x="${x}" y="${y}" text-anchor="middle" style="font-size: ${s}">${t}</text>`;
}
function drawLine(x1,y1,x2,y2,w=1.3,col = "black") {
    return `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" style="stroke: ${col}; stroke-width: ${w}px" />`;
}

// puzzle renderer ------------------------------

document.getElementById("subBtn").addEventListener("click", submitFunc);
function submitFunc() {
    document.getElementById("controls").style.display = "none";
    document.getElementById("requery").style.display = "block";
    showText("querying solution<br><i>too long? try 'solve without guessing'</i>", false);
    // set control ui

    var puzzle = `${clean(document.getElementById("textArRow").value)}\ne\n${clean(document.getElementById("textArCol").value)}\ne\n`;
    showTestCase(puzzle);

    fetch(`${URL}solve`,{
        headers: { "Content-type": "application/x-www-form-urlencoded" },
        body: `text=${puzzle}&solv=${solver}`,
        method:"POST"
    })
    .then(data => data.json())
    .then(res => res.length === 0 ? showText("puzzle has no solution") : printCanvas(res, puzzle))
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