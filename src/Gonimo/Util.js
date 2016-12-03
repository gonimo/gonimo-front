
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
