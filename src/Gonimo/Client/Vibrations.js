

exports._startVibration = function (vals) {
    return function (interval) {
        return function () {
            var vibrate = navigator.vibrate || navigator.mozVibrate || navigator.webkitVibrate;
            var vibrator = setInterval(function() {
                try {
                    vibrate(vals);
                }
                catch (e) {
                    console.log("Enabling vibrations failed!");
                }
            }, interval);
            return vibrator;
        };
    };
};

exports.stopVibration = function (vibrator) {
    return function () {
        clearInterval(vibrator);
        var vibrate = navigator.vibrate || navigator.mozVibrate || navigator.webkitVibrate;
        vibrate(0);
    };
};
