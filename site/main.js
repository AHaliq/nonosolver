function submitFunc() {
    var data = document.getElementById("textAr").value.replace(/\n/g, "nn");
    data = "text="+data+"nnenn";
    
    var xhr = new XMLHttpRequest();
    xhr.withCredentials = true;

    xhr.addEventListener("readystatechange", function () {
    if (this.readyState === 4) {
        //console.log(this.responseText);
        document.getElementById("textAr").value = this.responseText;
    }
    });

    xhr.open("POST", "http://localhost:8000/solve");
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    xhr.setRequestHeader("cache-control", "no-cache");
    xhr.setRequestHeader("Postman-Token", "cb805568-b265-4674-a928-9c4e9c200bbd");

    xhr.send(data);
}