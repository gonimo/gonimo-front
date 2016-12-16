
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


// Reuse the same audio context, because the number of audio contexts to allocate is limited.
var alertAudioContext = new window.AudioContext();
var decodedSound = null;

exports.boostVolumeMediaStream = function (stream) {
    return function () {
        var ctx = alertAudioContext;
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

                function makeMyAudio() {
                    var ctx = alertAudioContext;
                    var source = ctx.createBufferSource();
                    source.buffer = decodedSound;
                    source.connect(ctx.destination);
                    source.loop = true;
                    return source;
                }

                if (decodedSound == null) {
                    var request = new XMLHttpRequest();
                    request.open('GET', url, true);
                    request.responseType = 'arraybuffer';
                    // Decode asynchronously
                  request.onload = function() {
                      var ctx = alertAudioContext;
                      ctx.decodeAudioData(request.response, function(buffer) {
                          decodedSound = buffer;
                          success(makeMyAudio())();
                      }, function(e) { console.log ("Error:" +  e.message); error(e);});
                  };
                  request.send();
                }
                else {
                    success(makeMyAudio())();
                }
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

