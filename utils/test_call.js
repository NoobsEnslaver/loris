var host = window.location.origin;

var connection;
var token = "";

var loginPage = document.querySelector('#login-page'),
    msisdnInput = document.querySelector('#msisdn'),
    codeInput = document.querySelector('#code'),
    loginButton = document.querySelector('#login'),
    callPage = document.querySelector('#call-page'),
    theirMSISDNInput = document.querySelector('#their-msisdn'),
    callButton = document.querySelector('#call'),
    hangUpButton = document.querySelector('#hang-up'),
    sendsmsButton = document.querySelector('#send_sms');

var yourVideo = document.querySelector('#yours'),
    theirVideo = document.querySelector('#theirs'),
    yourConnection, connectedUser, stream, my_turn;

callPage.style.display = "none";

// Login when the user clicks the button
loginButton.addEventListener("click", function (event) {
        var xhr = new XMLHttpRequest();
        var body = {msisdn: parseInt(msisdnInput.value), sms_code: parseInt(codeInput.value)};

        xhr.open("POST", host + "/v3/auth/json", false);
        xhr.onloadend = function (e) {
                var resp = e.target.response;
                token = JSON.parse(resp).token;
                console.log("sucessfuly authorized, token: ", token);

                loginPage.style.display = "none";
                callPage.style.display = "block";
                // Get the plumbing ready for a call
                startConnection();
        };
        xhr.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
        xhr.send(JSON.stringify(body));

        var url = "wss://" + window.location.host + "/session/" + token + "/ws/v1/chat";
        connection = new WebSocket(url, ["msgpack"]);
        connection.binaryType = "arraybuffer";

        connection.onopen = function () {
            console.log("Connected");
            send({msg_type: 39});
        };

        connection.onmessage = function (message) {
                var Data = new Uint8Array(message.data);
                var data = msgpack.decode(Data);
                console.log("Got message", data);
                switch(data.msg_type) {
                case 119:
                    var their_turn = {
                        "iceServers": [{
                            url: 'turn:' + data.turn_server.adress + ':' + data.turn_server.port,
                            username: data.turn_server.username,
                            credential: data.turn_server.credential,
                            credentialType: data.turn_server.credential_type
                        }]
                    };
                    send({msg_type: 35});
                    if(window.confirm("Receive call from " + data.msisdn + " ?")){
                        onOffer(data.msisdn, {type: "offer", sdp: data.sdp}, their_turn);
                    } else{
                        hangUpButton.click();
                    }

                    break;
                case 120:
                        onAck();
                        break;
                case 121:
                        onCandidate(JSON.parse(data.candidate));
                        break;
                case 122:
                        onLeave(data.code);
                        break;
                case 123:
                        onAnswer({type: "answer", sdp: data.sdp});
                        break;
                case 124:
                    my_turn = {
                        "iceServers": [{
                            url: 'turn:' + data.adress + ':' + data.port,
                            username: data.username,
                            credential: data.credential,
                            credentialType: data.credential_type
                        }]
                    };
                    break;
                default:
                        console.log("unexpected msg: ", data);
                        break;
                }
        };

        connection.onerror = function (err) {
                console.log("Got error", err);
        };
});

sendsmsButton.addEventListener("click", function (event) {
        var xhr = new XMLHttpRequest();
        var body = {msisdn: parseInt(msisdnInput.value)};
        xhr.open("POST", host + "/v1/sms", true);
        xhr.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
        xhr.send(JSON.stringify(body));
        xhr.onloadend = function () {
                console.log("sms sucessfuly sended");
        };
});

function onAck(){
    return {};
}

function send(message) {
        var serialized_data = msgpack.encode(message);
        connection.send(serialized_data);
};

callButton.addEventListener("click", function () {
        var theirUsername = parseInt(theirMSISDNInput.value);
        startPeerConnection(theirUsername);
});

hangUpButton.addEventListener("click", function () {
        send({
                msg_type: 37,
                code: 200
        });
        onLeave();
});

function onOffer(name, offer, turn) {
        setupPeerConnection(turn);
        connectedUser = name;
        yourConnection.setRemoteDescription(new RTCSessionDescription(offer));
        yourConnection.createAnswer(function (answer) {
                yourConnection.setLocalDescription(answer);
                send({
                        msg_type: 38,
                        sdp: answer.sdp
                });
        }, function (error) {
                alert("An error has occurred");
        });
};

function onAnswer(answer) {
        yourConnection.setRemoteDescription(new RTCSessionDescription(answer));
};

function onCandidate(candidate) {
        yourConnection.addIceCandidate(new RTCIceCandidate(candidate));
};

function onLeave(Code) {
        connectedUser = null;
        theirVideo.src = null;
        yourConnection.close();
        yourConnection.onicecandidate = null;
        yourConnection.onaddstream = null;
    setupPeerConnection(my_turn);
        console.log("Connection closed with code: %o", Code);
};

function hasUserMedia() {
        navigator.getUserMedia = navigator.getUserMedia ||
                navigator.webkitGetUserMedia || navigator.mozGetUserMedia ||
                navigator.msGetUserMedia;
        return !!navigator.getUserMedia;
};

function hasRTCPeerConnection() {
        window.RTCPeerConnection = window.RTCPeerConnection ||
                window.webkitRTCPeerConnection || window.mozRTCPeerConnection;
        window.RTCSessionDescription = window.RTCSessionDescription ||
                window.webkitRTCSessionDescription ||
                window.mozRTCSessionDescription;
        window.RTCIceCandidate = window.RTCIceCandidate ||
                window.webkitRTCIceCandidate || window.mozRTCIceCandidate;
        return !!window.RTCPeerConnection;
};

function startConnection() {
        if (hasUserMedia()) {
                navigator.getUserMedia({ video: true, audio: true }, function (myStream) {
                        stream = myStream;
                    //yourVideo.src = window.URL.createObjectURL(stream);
                        if (hasRTCPeerConnection()) {

                        } else {
                                alert("Sorry, your browser does not support WebRTC.");
                        }
                }, function (error) {
                        console.log(error);
                });
        } else {
                alert("Sorry, your browser does not support WebRTC.");
        }
};


function setupPeerConnection(config) {
        yourConnection = new RTCPeerConnection(config);
        // Setup stream listening
        yourConnection.addStream(stream);
        yourConnection.onaddstream = function (e) {
                theirVideo.src = window.URL.createObjectURL(e.stream);
        };
        // Setup ice handling
        yourConnection.onicecandidate = function (event) {
                if (event.candidate) {
                        send({
                                msg_type: 36,
                                candidate: JSON.stringify(event.candidate)
                        });
                }
        };
};

function startPeerConnection(user) {
        connectedUser = user;
        setupPeerConnection(my_turn);
        // Begin the offer
        yourConnection.createOffer(function (offer) {
                send({
                        msg_type: 34,
                        msisdn: user,
                        sdp: offer.sdp
                });
                yourConnection.setLocalDescription(offer);
        }, function (error) {
                alert("An error has occurred.");
        });
};
