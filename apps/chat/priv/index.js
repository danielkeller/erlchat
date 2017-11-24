function random_text(len) {
    var chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    var out = "";
    for (var i = 0; i < len; ++i)
        out += chars[Math.floor(Math.random()*chars.length)];
    return out;
}

$("#go").on('click', function () {
    window.location = "/c/" + random_text(10);
})
