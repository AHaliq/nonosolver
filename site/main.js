showText("1 1\n1\n1 1\ne\n1 1\n1\n1 1\ne\n");
function showText(str, area = true) {
    if(area) {
        document.getElementById("textAr").value = str;
    }else {
        document.getElementById("outAr").innerHTML = str;
    }
}
function submitFunc() {
    var data = document.getElementById("textAr").value.replace(/\n/g, "nn");
    data = "text="+data;
    
    fetch("http://localhost:8000/solve",{
        headers:{
            "content-type": "application/x-www-form-urlencoded",
            "cache-control": "no-cache",
            "Postman-Token": "cb805568-b265-4674-a928-9c4e9c200bbd"
        },
        body:data,
        method:"POST"
    })
    .then(data => data.json())
    .then(res => {
        //showText(res.replace(/\n/g,"<br/>"), false)
        showText(res.reduce((a,c,i) => a + matToStr(i+1,c), ""), false)
    })
    .catch(err=>console.log(error));
}
function matToStr(i,m) {
    var str = "Soln " + i + ": <br /><br />";
    for(let i = 0; i < m.length; i++) {
        for(let j = 0; j < m[i].length; j++) {
            str += m[i][j];
        }
        str += "<br />";
    }
    str += "<br />"
    return str;
}