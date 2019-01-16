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
    .then(res => showText(
        res.reduce((a,c,i) => a + /*"Soln " + i + ": <br /><br />" +*/
            c.reduce((f,g) => f + g.reduce((b,d) => b + d) + "<br />","") + "<br />", "")
        , false))
    .catch(err=>console.log(err));
}