var socket;

document.getElementById("reconnect").onclick = function(e){
        var url = document.getElementById("url").value;
        socket = new WebSocket(url, ["msgpack"]);
        socket.binaryType = "arraybuffer";

        socket.onopen = function() {
                document.getElementById("status").textContent = "OPEN";
                document.getElementById("status").style.color = "green";
                console.log("connected");
        };

        socket.onclose = function() {
                document.getElementById("status").textContent = "CLOSE";
                document.getElementById("status").style.color = "red";
                console.log("disconnected");
        }
        ;
        socket.onerror = function(s) {
                //document.getElementById("error").textContent = s.text;
                console.error("%o", s);
        };

        socket.onmessage= function(s) {
                var Data = new Uint8Array(s.data);
                var Transport = document.getElementById("transport").value;
                var Resp;
                switch(Transport){
                case "msgpack":
                        var MsgObj = msgpack.decode(Data);
                        console.log("%o", MsgObj);
                        Resp = JSON.stringify(MsgObj);
                        break;
                case "text":
                        Resp = s.data;
                        break;
                default:
                        throw("Unknown transport");
                };
                document.getElementById("resp").value = Resp;
        };

};

document.getElementById("send").onclick = function(e){
        var TextMsg = document.getElementById("req").value;
        var Transport = document.getElementById("transport").value;
        var Msg;
        switch(Transport){
                case "msgpack":
                        var RawMsg = JSON.parse(TextMsg);
                        Msg = msgpack.encode(RawMsg);
                break;
                case "text":
                        Msg = TextMsg;
                break;
                default:
                        throw("Unknown transport");
        }
        socket.send(Msg);
};

document.getElementById("clear").onclick = function(e){
        document.getElementById("req").value = "";
        document.getElementById("resp").value = "";
};

document.getElementById("reconnect").click();
