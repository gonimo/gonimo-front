

exports._startVibration = function (vals) {
    return function (interval) {
        return function () {
            var vibrate = navigator.vibrate || navigator.mozVibrate || navigator.webkitVibrate;
            var vibrator = setInterval(function() {
                try {
                    vibrate.call(navigator, vals);
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
        vibrate.call(navigator, 0);
    };
};
