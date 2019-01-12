function submitFunc() {
    var data = document.getElementById("textAr").value.replace(/\n/g, "nn");
    data = "text="+data+"nnenn";
    
    fetch("http://localhost:8000/solve",{
        headers:{
            "content-type": "application/x-www-form-urlencoded",
            "cache-control": "no-cache",
            "Postman-Token": "cb805568-b265-4674-a928-9c4e9c200bbd"
        },
        body:data,
        method:"POST"
    })
    .then(data=>data.text())
    .then(res=>{
        document.getElementById("textAr").value = res;
    }).catch(err=>console.log(error));
}