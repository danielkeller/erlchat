function insertMessage(msg) {
    latest = Math.max(latest, msg.i);
    var icon = $('<svg class="icon" width="20" height="20">').jdenticon(msg.u);
    $('#chat_data').append(icon, $('<p>').text(msg.m));
    $('html').scrollTop(Number.MAX_SAFE_INTEGER);
}

var chatId = window.location.pathname.substr(-10);
var latest = 0;
var retries = 3;

function openConnection() {
    var pull = new XMLHttpRequest();
    var seenBytes = 0;
    pull.open('GET', '/chat/' + chatId + '/' + latest);
    pull.onreadystatechange = function() {
        if(pull.readyState == 3) {
            for (var end = seenBytes + 1; end < pull.response.length;) {
                if (pull.response[end] == '}') {
                    ++end;
                    var newData = pull.response.substr(seenBytes, end - seenBytes);
                    seenBytes = end;
                    insertMessage(JSON.parse(newData));
                }
                else
                    ++end;
            }
        }
    };
    pull.onerror = function () {
        if (retries--)
            setTimeout(openConnection, 200);
        else
            $('#chat_data').text("It's broken, sorry");
    }
    pull.send();
}

openConnection();

$('#send').on('submit', function (event) {
    event.preventDefault();
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/chat/' + chatId);
    xhr.send($('#text').val());
    $('#text').val('');
});