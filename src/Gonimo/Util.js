
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
