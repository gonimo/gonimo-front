
exports.createUrlSignal = function locationChanged(constant) {
    var url = "";
    if (typeof window !== 'undefined') {
        url = window.location.href
    }
    var out = constant(url);
    if (typeof window !== 'undefined') {
        window.onpopstate = function () {
            out.set(window.location.href);
        };
    }
    return function () {
        return out;
    };
};

exports.differentObject = function(obj1) {
    return function(obj2) {
        return (obj1 !== obj2);
    };
};


exports.boostVolumeMediaStream = function (stream) {
    return function () {
        var ctx = new window.AudioContext();
        var source = ctx.createMediaStreamSource(stream);
        var gainNode = ctx.createGain();
        gainNode.gain.value = 10;
        source.connect(gainNode);
        // gainNode.connect(ctx.destination);
        var destNode = ctx.createMediaStreamDestination();
        gainNode.connect(destNode);
        var outStream = destNode.stream;
        var videoTracks = stream.getVideoTracks();
        for(var i=0; i < videoTracks.length; i++) {
            outStream.addTrack(videoTracks[i]);
        }
        return outStream;
    };
};

exports._loadSound = function (success) {
    return function (error) {
        return function (url) {
            return function () {
                var request = new XMLHttpRequest();
                request.open('GET', url, true);
                request.responseType = 'arraybuffer';

                // Decode asynchronously
                console.log("ok - setting on load");
                request.onload = function() {
                    console.log("Ok received request");
                    var ctx = new window.AudioContext();
                    ctx.decodeAudioData(request.response, function(buffer) {
                        console.log("ok audio decoded!");
                        var source = ctx.createBufferSource();
                        source.buffer = buffer;
                        source.connect(ctx.destination);
                        source.loop = true;
                        console.log("Ok calling success");
                        success(source)();
                        console.log("Ok called success");
                    }, function(e) { console.log ("Error:" +  e.message); error(e);});
                };
                console.log("ok - sending request");
                request.send();
            };
        };
    };
};

exports.playSound = function (myAudio) {
    return function () {
        myAudio.start();
    };
};

exports.stopSound = function (myAudio) {
    return function () {
        myAudio.stop();
    };
};
