function toMessage(msg) {
    latest = Math.max(latest, msg.i);
    earliest = Math.min(earliest, msg.i);
    updateScrollback();
    var icon = $('<svg class="icon" width="20" height="20">').jdenticon(msg.u);
    return [icon, $('<p>').text(msg.m)];
}

function appendMessage(msg) {
    $('#chat_data').append(toMessage(msg));
    $('html').scrollTop(Number.MAX_SAFE_INTEGER);
}

function prependMessage(msg) {
    $('#chat_data').prepend(toMessage(msg));
}

function updateScrollback() {
    if (earliest === 1)
        $('#scrollback').text("No more messages");
    else {
        var btn = $('<button>See earlier</button>').on('click', scrollback);
        $('#scrollback').html(btn);
    }
}

function scrollback() {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/chat/' + chatId + '/old/' + earliest);
    xhr.onload = function () {
        var msgs = JSON.parse(xhr.response);
        msgs.reverse().map(prependMessage);
    }
    xhr.send();
}

var chatId = window.location.pathname.substr(-10);
var earliest = Number.MAX_SAFE_INTEGER;
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
                    appendMessage(JSON.parse(newData));
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